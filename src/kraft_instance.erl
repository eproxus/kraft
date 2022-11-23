-module(kraft_instance).

-behavior(gen_server).

% API
-export([start/1]).
-ignore_xref([{?MODULE, start_link, 1}]).
-export([start_link/1]).
-export([stop/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-include_lib("kernel/include/logger.hrl").

%--- API -----------------------------------------------------------------------

start(Params) -> kraft_instance_sup:start_instance(Params).

start_link(Params) -> gen_server:start_link(?MODULE, Params, []).

stop(Ref) -> kraft_instance_sup:stop_instance(Ref).

%--- Callbacks -----------------------------------------------------------------

init(#{app := App, owner := Owner, opts := #{port := Port} = Opts} = Params) ->
    link(Owner),
    % Create routes
    InternalRoutes = {"/kraft", kraft_static, #{app => kraft}},
    AllRoutes = routes(App, [InternalRoutes | maps:get(routes, Params)]),
    Dispatch = cowboy_router:compile([{'_', lists:flatten(AllRoutes)}]),
    persistent_term:put({kraft_dispatch, App}, Dispatch),

    % Start Cowboy
    ListenerName = listener_name(App),
    ProtocolOpts = #{
        env => #{dispatch => {persistent_term, {kraft_dispatch, App}}},
        stream_handlers => [
            kraft_fallback_h,
            cowboy_compress_h,
            cowboy_stream_h
        ]
    },
    {ok, Listener} =
        case Opts of
            #{ssl_opts := SslOpts} ->
                TransportOpts = [{port, Port} | SslOpts],
                cowboy:start_tls(ListenerName, TransportOpts, ProtocolOpts);
            _ ->
                TransportOpts = [{port, Port}],
                cowboy:start_clear(ListenerName, TransportOpts, ProtocolOpts)
        end,
    link(Listener),

    ?LOG_NOTICE(#{started => #{port => Port}}, #{kraft_app => kraft}),

    {ok, Listener}.

handle_call(Request, From, _State) -> error({unknown_request, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info(Info, _State) -> error({unknown_info, Info}).

terminate(_Reason, Listener) ->
    cowboy:stop_listener(Listener).

%--- Internal ------------------------------------------------------------------

listener_name(App) ->
    list_to_atom("kraft_listener_" ++ atom_to_list(App)).

routes(App, Routes) ->
    lists:flatmap(
        fun(Route) ->
            Attr = element(3, Route),
            AppName =
                case Attr of
                    #{app := RouteApp} -> RouteApp;
                    _Else -> App
                end,
            kraft_dev:watch(AppName),
            route(Route, AppName)
        end,
        Routes
    ).

route({Path, {ws, Handler}, MState}, App) ->
    route({Path, {ws, Handler}, MState, #{}}, App);
route({Path, {ws, Handler}, MState, Opts}, App) ->
    {Module, State} = kraft_ws_util:setup(Opts, App, Handler, MState),
    [{Path, Module, State}];
route({Path, kraft_static, #{file := File}}, App) ->
    StaticFile = filename:join("web/static/", File),
    [{Path, cowboy_static, {priv_file, App, StaticFile}}];
route({Path, kraft_static, _State}, App) ->
    static_routes(App, Path);
route({Path, {cowboy, Handler}, State}, _App) ->
    [{Path, Handler, State}];
route({Path, Handler, State}, App) ->
    [
        {Path, kraft_controller, #{
            handler => Handler,
            app => App,
            state => State
        }}
    ].

static_routes(App, Path) ->
    Default = [
        {
            uri_join(Path, "[...]"),
            cowboy_static,
            {priv_dir, App, "web/static"}
        }
    ],
    Static = kraft_file:path(App, static),
    Context = {Static, App, Path},
    StaticRoute = fun(File, Acc) -> static_route(File, Context, Acc) end,
    filelib:fold_files(Static, ".*", true, StaticRoute, Default).

static_route(File, {Static, App, Path}, Acc) ->
    Prefix = string:trim(string:prefix(File, Static), leading, "/"),
    case filename:basename(Prefix) of
        "index.html" ->
            PrivFile = {priv_file, App, filename:join("web/static/", Prefix)},
            SubDir =
                case filename:dirname(Prefix) of
                    "." -> "";
                    Dir -> Dir
                end,
            IndexPath = uri_join(Path, SubDir),
            [{IndexPath, cowboy_static, PrivFile} | Acc];
        _ ->
            Acc
    end.

uri_join(Path, SubPath) ->
    Prefix = string:trim(Path, trailing, "/"),
    Suffix = string:trim(SubPath, leading, "/"),
    string:join([Prefix, Suffix], "/").
