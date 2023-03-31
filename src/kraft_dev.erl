-module(kraft_dev).

-behavior(gen_server).

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

handle(App, Dir, #{path := Path, written := [File]}) ->
    handle_file(App, Dir, Path, File, written);
handle(App, Dir, #{path := Path, rename := [File]}) ->
    handle_file(App, Dir, Path, File, rename);
handle(App, Dir, #{path := Path, removed := [File]}) ->
    handle_file(App, Dir, Path, File, removed);
handle(App, Dir, Event) when map_size(Event) == 0 ->
    ?LOG_NOTICE(#{monitor => Dir, event => started}, #{kraft_app => App}).

handle_file(App, Dir, Path, File, Event) ->
    Full = filename:join(Path, File),
    Relative = string:prefix(Full, Dir),
    RootName = filename:rootname(Relative, <<".mustache">>),
    try
        case filelib:is_regular(Full) of
            false -> kraft_template:remove(App, RootName);
            true -> kraft_template:reload(App, RootName)
        end
    catch
        Class:Reason:ST ->
            ?LOG_ERROR(#{
                class => Class,
                reason => Reason,
                event => Event,
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
        " -w " ++ Dir,
        " --no-vcs-ignore",
        " --no-meta",
        " --",
        "  echo '" ++ watchexec_line() ++ "'"
    ].

watchexec_line() ->
    "\\\"$WATCHEXEC_COMMON_PATH\\\" "
    "\\\"$WATCHEXEC_CREATED_PATH\\\" "
    "\\\"$WATCHEXEC_REMOVED_PATH\\\" "
    "\\\"$WATCHEXEC_RENAMED_PATH\\\" "
    "\\\"$WATCHEXEC_WRITTEN_PATH\\\" "
    "\\\"$WATCHEXEC_OTHERWISE_CHANGED_PATH\\\"".

watchexec_line_event(Line) ->
    [Path, Created, Removed, Renamed, Written, Other] =
        [split_paths(Paths) || Paths <- string:lexemes(Line, [$\s])],
    Attrs = [
        {path, Path},
        {created, Created},
        {removed, Removed},
        {rename, Renamed},
        {written, Written},
        {other, Other}
    ],
    maps:from_list([Elem || Elem = {_, Value} <- Attrs, Value =/= []]).

split_paths(Paths) -> string:lexemes(string:trim(Paths, both, "\""), [$:]).
