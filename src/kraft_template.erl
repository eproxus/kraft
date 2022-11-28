-module(kraft_template).

-behavior(gen_server).

-include_lib("kernel/include/logger.hrl").

% API
-export([start_link/0]).
-ignore_xref(start_link/0).
-export([render/3]).
-export([reload/2]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%--- API -----------------------------------------------------------------------

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

render(App, RawFile, Context) ->
    File = trim(RawFile),
    Template = kraft_cache:retrieve(template, [App, File], fun() ->
        parse(App, File)
    end),
    compile(Template, Context).

reload(App, File) -> reload(App, File, main).

reload(App, RawFile, Level) ->
    File = trim(RawFile),
    Template = parse(App, File),
    kraft_cache:update(template, [App, File], Template),
    ?LOG_INFO(#{template => File, event => reloaded, level => Level}, #{
        kraft_app => App
    }),
    [reload(App, D, dep) || [D] <- deps(App, File)],
    ok.

%--- Callbacks -----------------------------------------------------------------

init(undefined) ->
    ets:new(?MODULE, [named_table, bag, public]),
    kraft_cache:init(template),
    {ok, #{}}.

handle_call(Request, From, _State) -> error({unknown_request, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info(Info, _State) -> error({unknown_info, Info}).

%--- Internal ------------------------------------------------------------------

trim(File) -> string:trim(File, leading, [$/]).

parse(App, File) ->
    Path = kraft_file:path(App, template, File),
    try
        PartialFileReader = fun(Dir, Key) ->
            deps_insert(App, File, Key),
            bbmustache:default_partial_file_reader(Dir, Key)
        end,
        bbmustache:parse_file(Path, [{partial_file_reader, PartialFileReader}])
    catch
        error:file_not_found ->
            error({missing_template, App, kraft_file:relative(App, File)})
    end.

compile(Template, Context) ->
    bbmustache:compile(Template, Context, [{key_type, atom}]).

deps_insert(App, File, Key) ->
    Relative =
        case filename:dirname(File) of
            <<".">> -> Key;
            Dir -> filename:join(Dir, Key)
        end,
    ets:insert(?MODULE, {{App, Relative}, File}).

deps(App, File) -> ets:match(?MODULE, {{App, File}, '$1'}).
