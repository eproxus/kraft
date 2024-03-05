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
    % FIXME: Use monitor instead
    link(Owner),
    % Trap exits so terminate/2 gets called
    % (see  https://www.erlang.org/doc/man/gen_server.html#Module:terminate-2)
    process_flag(trap_exit, true),
    % Create routes
    Dispatch = compile_cowboy(App, maps:get(routes, Params), Opts),
    DispatchKey = {kraft_dispatch, App, make_ref()},
    persistent_term:put(DispatchKey, Dispatch),

    % Start Cowboy
    ListenerName = listener_name(App),
    ProtocolOpts = #{
        env => #{dispatch => {persistent_term, DispatchKey}},
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
    % FIXME: Use monitor instead
    link(Listener),

    ?LOG_NOTICE(#{started => #{port => Port}}, #{kraft_app => kraft}),

    {ok, ListenerName}.

handle_call(Request, From, _State) -> error({unknown_request, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

% TODO: Handler owner/Cowboy crashes here
handle_info(Info, _State) -> error({unknown_info, Info}).

terminate(_Reason, ListenerName) ->
    cowboy:stop_listener(ListenerName).

%--- Internal ------------------------------------------------------------------

compile_cowboy(App, Routes, #{ssl_opts := SslOpts}) ->
    case proplists:is_defined(sni_hosts, SslOpts) of
        true -> compile_cowboy_with_sni(App, Routes);
        false -> compile_cowboy(App, Routes)
    end;
compile_cowboy(App, Routes, _Opts) ->
    compile_cowboy(App, Routes).

compile_cowboy(App, Routes) ->
    InternalRoutes = {"/kraft", kraft_static, #{app => kraft}},
    AllRoutes = routes(App, [InternalRoutes | Routes]),
    cowboy_router:compile([{'_', lists:flatten(AllRoutes)}]).

compile_cowboy_with_sni(App, Routes) ->
    InternalRoutes = {"/kraft", kraft_static, #{app => kraft}},
    AllRoutes = sni_routes(App, Routes, InternalRoutes),
    cowboy_router:compile(AllRoutes).

listener_name(App) -> {kraft_listener, App, make_ref()}.

sni_routes(App, Routes, InternalRoutes) ->
    lists:map(
        fun({Host, HostRoutes}) ->
            {Host, routes(App, [InternalRoutes | HostRoutes])}
        end,
        Routes
    ).

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
route({Path, kraft_static, #{file := File} = State}, App) ->
    StaticFile = filename:join("web/static/", File),
    Handler = maps:get(handler, State, cowboy_static),
    [{Path, Handler, {priv_file, App, StaticFile}}];
route({Path, kraft_static, State}, App) ->
    Handler = maps:get(handler, State, cowboy_static),
    static_routes(App, Path, Handler);
route({Path, {cowboy, Handler}, State}, _App) ->
    [{Path, Handler, State}];
route({Path, Handler, State}, App) ->
    route({Path, Handler, State, #{}}, App);
route({Path, Handler, State, Opts}, App) ->
    Mod =
        case Opts of
            #{type := controller} -> kraft_controller;
            #{type := Type} -> error({invalid_handler_type, Type});
            _ -> kraft_controller
        end,
    [
        {Path, kraft_handler, #{
            mod => Mod,
            handler => Handler,
            app => App,
            state => State,
            route => iolist_to_binary(Path)
        }}
    ].

static_routes(App, Path, Handler) ->
    Default = [
        {
            uri_join(Path, "[...]"),
            Handler,
            {priv_dir, App, "web/static"}
        }
    ],
    Static = kraft_file:path(App, static),
    Context = {Static, App, Path},
    StaticRoute = fun(File, Acc) ->
        static_route(File, Context, Handler, Acc)
    end,
    filelib:fold_files(Static, ".*", true, StaticRoute, Default).

static_route(File, {Static, App, Path}, Handler, Acc) ->
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
            [{IndexPath, Handler, PrivFile} | Acc];
        _ ->
            Acc
    end.

uri_join(Path, SubPath) ->
    Prefix = string:trim(Path, trailing, "/"),
    Suffix = string:trim(SubPath, leading, "/"),
    string:join([Prefix, Suffix], "/").
