-module(kraft_template).

-behavior(gen_server).

-include_lib("kernel/include/logger.hrl").

% API
-export([start_link/0]).
-ignore_xref(start_link/0).
-export([render/3]).
-export([reload/2]).
-export([remove/2]).
-export([response/3]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%--- Types ---------------------------------------------------------------------

-export_type([body_template/0]).

-type body_template() :: {template, file:name(), bbmustache:data()}.

%--- API -----------------------------------------------------------------------

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

render(Conn0, RawFile, Context) ->
    App = kraft_conn:'_meta'(Conn0, app),
    File = trim(RawFile),
    Template = kraft_cache:retrieve(template, [App, File], fun() ->
        parse(App, File)
    end),
    compile(Template, Context).

reload(App, RawFile) -> reload(App, trim(RawFile), main).

remove(App, RawFile) ->
    File = trim(RawFile),
    kraft_cache:clear(template, [App, File]),
    ?LOG_INFO(#{template => File, event => removed}, #{kraft_app => App}),
    ok.

response(Conn0, Template, Context) ->
    Body = render(Conn0, Template, Context),
    Conn1 = kraft_conn:response_body(Conn0, Body),
    kraft_conn:response_headers(Conn1, #{
        <<"content-type">> => mime_type(Template)
    }).

%--- Callbacks -----------------------------------------------------------------

init(undefined) ->
    ets:new(?MODULE, [named_table, bag, public]),
    kraft_cache:init(template),
    {ok, #{}}.

handle_call(Request, From, _State) -> error({unknown_request, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info(Info, _State) -> error({unknown_info, Info}).

%--- Internal ------------------------------------------------------------------

reload(App, File, Level) ->
    Template = parse(App, File),
    kraft_cache:update(template, [App, File], Template),
    ?LOG_INFO(#{template => File, event => reloaded, level => Level}, #{
        kraft_app => App
    }),
    [reload(App, D, dep) || [D] <- deps(App, File)],
    ok.

trim(File) -> string:trim(iolist_to_binary(File), leading, [$/]).

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
            error({missing_template, App, kraft_file:relative(App, Path)})
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

mime_type(File) ->
    {Type, SubType, []} = cow_mimetypes:all(iolist_to_binary(File)),
    [Type, $/, SubType].
