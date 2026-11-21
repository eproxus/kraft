-module(kraft_dev).

-behavior(gen_server).

-moduledoc """
Development mode utilities for Kraft framework.

## Features

* **Template Hot Reloading**: Automatically reload templates when files change
* **File System Monitoring**: Uses `watchexec` for efficient file watching
* **Dependency Tracking**: Reloads dependent templates when partials change
* **Development Mode Detection**: Automatically enabled in development environments
* **Error Handling**: Graceful handling of file system errors and missing files

## Requirements

The `watchexec` command-line tool must be installed for hot-reloading to work:

```bash
# macOS
brew install watchexec

# Ubuntu/Debian
sudo apt install watchexec

# Arch Linux
sudo pacman -S watchexec
```

## Development Mode Detection

Development mode is automatically enabled when:
* `#{mode => dev}` is explicitly set in server options
* Running under `rebar3` (detected automatically)
* `watchexec` is available in the system PATH

## Usage

Most common usage is to start the server with development mode:

```erlang
% Start server with development mode
kraft:start(#{port => 8080, mode => dev}, Routes).

% Or let it auto-detect (recommended)
kraft:start(#{port => 8080}, Routes).
```

## See Also

- `m:kraft_template` - Template rendering module
- `m:kraft_instance` - Server instance management
""".

% API
-export([maybe_start/1]).
-export([start_link/0]).
-export([watch/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").

% -compile(export_all).

-record(s, {
    watchexec = disabled,
    dirs = #{},
    monitors = #{}
}).

%--- API -----------------------------------------------------------------------

-doc """
Conditionally start development mode based on options.

This function checks if development mode should be enabled and starts
the development server if appropriate. It's called automatically by
`kraft:start/2` when development mode is detected.
""".
-spec maybe_start(#{mode => dev | prod} | map()) -> ok.
maybe_start(Opts) ->
    case mode(Opts) of
        dev ->
            {ok, Pid} = kraft_dev:start_link(),
            persistent_term:put(?MODULE, Pid);
        _Other ->
            ok
    end.

start_link() ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
    end.

-doc """
Watch a specific application's template directory.

Sets up file system monitoring for an application's template directory.
This function is called automatically by `kraft_instance` when a new
application is started. It monitors `priv/web/templates/` for changes.
""".
-spec watch(atom()) -> ok | {error, watchexec_disabled | enoent}.
watch(App) ->
    Dir = filename:join(code:lib_dir(App), "priv/web/templates"),
    case persistent_term:get(?MODULE, undefined) of
        undefined -> ok;
        Pid -> gen_server:call(Pid, {watch, App, resolve_symlinks(Dir)})
    end.

%--- Callbacks -----------------------------------------------------------------

init(undefined) ->
    State =
        case os:find_executable("watchexec") of
            false ->
                ?LOG_WARNING(#{
                    reason => watchexec_missing,
                    message => "Template reloading will be disabled"
                }),
                #s{};
            Path ->
                #s{watchexec = Path}
        end,
    {ok, State}.

handle_call({watch, App, Dir}, _From, S) when is_map_key(Dir, S#s.dirs) ->
    ?LOG_DEBUG(#{monitor => Dir, event => duplicate}, #{kraft_app => App}),
    {reply, ok, S};
handle_call({watch, _App, _Dir}, _From, #s{watchexec = disabled} = S) ->
    {reply, {error, watchexec_disabled}, S};
handle_call({watch, App, Dir}, _From, #s{dirs = Dirs} = S) ->
    case filelib:is_dir(Dir) of
        false ->
            {reply, {error, enoent}, S};
        true ->
            Port = watchexec_start(Dir),
            {reply, ok, S#s{
                monitors = maps:put(
                    Port, #{app => App, dir => Dir}, S#s.monitors
                ),
                dirs = maps:put(Dir, Port, Dirs)
            }}
    end;
handle_call(Request, From, _State) ->
    error({unknown_request, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info({Port, {data, {eol, Line}}}, S) ->
    Event = watchexec_line_event(Line),
    #{app := App, dir := Dir} = maps:get(Port, S#s.monitors),
    handle(App, Dir, Event),
    {noreply, S};
handle_info({Port, {exit_status, Status}}, S) ->
    #{app := App, dir := Dir} = maps:get(Port, S#s.monitors),
    ?LOG_ERROR(
        #{monitor => Dir, event => {watcher_error, Port, {exit, Status}}},
        #{kraft_app => App}
    ),
    {noreply, S#s{
        monitors = maps:remove(Port, S#s.monitors),
        dirs = maps:remove(Dir, S#s.dirs)
    }};
handle_info(Info, _State) ->
    error({unknown_info, Info}).

%--- Internal ------------------------------------------------------------------

handle(_App, _Dir, #{path := #{filetype := <<"dir">>}}) ->
    ok;
handle(App, Dir, #{path := #{absolute := Path}}) ->
    handle_file(App, Dir, Path).

handle_file(App, Dir, File) ->
    Relative = string:prefix(File, Dir),
    RootName = filename:rootname(Relative, <<".mustache">>),
    try
        case filelib:is_regular(File) of
            false -> kraft_template:remove(App, RootName);
            true -> kraft_template:reload(App, RootName)
        end
    catch
        error:{missing_template, App, Path} ->
            ?LOG_DEBUG(#{
                app => App,
                message => "Ignoring unknown template",
                file => File,
                path => Path,
                relative => Relative
            });
        Class:Reason:ST ->
            ?LOG_ERROR(#{
                app => App,
                class => Class,
                reason => Reason,
                file => File,
                message => "Could not reload template",
                stacktrace => ST
            })
    end.

mode(#{mode := Mode}) ->
    Mode;
mode(_Opts) ->
    case code:is_loaded(rebar3) of
        {file, _File} -> dev;
        _Else -> prod
    end.

resolve_symlinks(Path) ->
    resolve_symlinks(filename:split(Path), []).

resolve_symlinks([], Stack) ->
    filename:join(Stack);
resolve_symlinks([Dir | Path], Stack) ->
    Current = Stack ++ [Dir],
    CurrentDir = filename:join(Current),
    case file:read_link_info(CurrentDir) of
        {ok, #file_info{type = symlink}} ->
            {ok, Dest} = file:read_link(CurrentDir),
            FullDest = filename:absname(
                filename:join(Stack ++ filename:split(Dest))
            ),
            Abs = absolute_path(filename:split(FullDest)),
            resolve_symlinks(Abs ++ Path, []);
        _ ->
            resolve_symlinks(Path, Current)
    end.

absolute_path(Path) -> absolute_path(Path, []).

absolute_path([], Acc) ->
    lists:reverse(Acc);
absolute_path([".." | Path], [Prev | Acc]) when Prev =/= ".." ->
    absolute_path(Path, Acc);
absolute_path([Dir | Path], Acc) ->
    absolute_path(Path, [Dir | Acc]).

watchexec_start(Dir) ->
    Command = lists:flatten(watchexec_cmd(Dir)),
    ?LOG_DEBUG("Starting watchexec: ~p", [Command]),
    erlang:open_port(
        {spawn, Command},
        [{line, 1024}, exit_status, use_stdio, binary, stderr_to_stdout]
    ).

watchexec_cmd(Dir) ->
    [
        "watchexec",
        " --no-discover-ignore",
        " --no-meta",
        " --only-emit-events",
        " --emit-events-to json-stdio",
        " --watch ",
        Dir
    ].

watchexec_line_event(Line) ->
    #{tags := Tags} = kraft_json:decode(Line),
    #{
        binary_to_atom(Kind) => maps:without([Kind], Tag)
     || #{kind := Kind} = Tag <- Tags
    }.
