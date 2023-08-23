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

init(#{app := App, owner := Owner, listeners := ListenerSpecs}) ->
    % FIXME: Use monitor instead
    link(Owner),
    % Trap exits so terminate/2 gets called
    % (see  https://www.erlang.org/doc/man/gen_server.html#Module:terminate-2)
    process_flag(trap_exit, true),
    Listeners = maps:map(
        fun(Addr, Params) -> start_listener(App, Addr, Params) end,
        ListenerSpecs
    ),
    {ok, Listeners}.

handle_call(Request, From, _State) -> error({unknown_request, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};
handle_info(Info, _State) ->
    error({unknown_info, Info}).

terminate(_Reason, Listeners) ->
    maps:foreach(
        fun(_Addr, #{listener := Name, persistent_terms := PTerms}) ->
            ok = cowboy:stop_listener(Name),
            [true = persistent_term:erase(T) || T <- PTerms]
        end,
        Listeners
    ).

%--- Internal ------------------------------------------------------------------

start_listener(App, Address, #{routes := Routes} = Params) ->
    ListenerRef = make_ref(),

    #{scheme := Scheme, host := Host, port := Port} = uri(Address),

    % Routes
    InternalRoutes = {"/kraft", kraft_static, #{app => kraft}},
    AllRoutes = routes(App, [InternalRoutes | Routes]),
    Dispatch = cowboy_router:compile([
        {route_host(Host), lists:flatten(AllRoutes)}
    ]),
    RoutesKey = {kraft_dispatch, App, ListenerRef},
    persistent_term:put(RoutesKey, Dispatch),

    % Cowboy
    ListenerName = {kraft_listener, App, ListenerRef},
    ProtocolOpts = #{
        env => #{dispatch => {persistent_term, RoutesKey}},
        stream_handlers => [
            kraft_fallback_h,
            cowboy_compress_h,
            cowboy_stream_h
        ]
    },
    {Start, ExtraTransportOpts} =
        % FIXME: Use Address to calculate opts
        case Scheme of
            <<"http">> -> {start_clear, []};
            <<"https">> -> {start_tls, maps:get(ssl_opts, Params, [])}
        end,
    TransportOpts = [{port, Port} | ExtraTransportOpts],
    {ok, ListenerPid} = cowboy:Start(ListenerName, TransportOpts, ProtocolOpts),
    % FIXME: Use monitor instead
    link(ListenerPid),

    ?LOG_NOTICE(#{started => #{port => Port}}, #{kraft_app => kraft}),
    #{listener => ListenerName, persistent_terms => [RoutesKey]}.

uri(RawAddress) when is_map(RawAddress) ->
    Address = maps:map(
        fun
            (scheme, <<"http">> = Scheme) -> Scheme;
            (scheme, <<"https">> = Scheme) -> Scheme;
            (scheme, http) -> <<"http">>;
            (scheme, https) -> <<"https">>;
            (scheme, _) -> error({invalid_listener_scheme, RawAddress});
            (host, Host) when is_binary(Host) -> Host;
            (host, _) -> error({invalid_listener_host, RawAddress});
            (port, Port) when is_integer(Port) -> Port;
            (port, _) -> error({invalid_listener_port, RawAddress});
            (_, _) -> error({invalid_listener_address, RawAddress})
        end,
        RawAddress
    ),
    case maps:merge(#{host => <<"*">>}, Address) of
        #{scheme := _, port := _} = A -> A;
        #{scheme := <<"https">>} = A -> A#{port => 443};
        #{scheme := <<"http">>} = A -> A#{port => 80};
        #{port := 443} = A -> A#{scheme => <<"https">>};
        #{port := _} = A -> A#{scheme => <<"http">>}
    end;
% Address -> Expanded
%   localhost          -> http://localhost:80
%   example.com        -> http://example.com:80
%   :443               -> https://*:443
%   http://example.com -> http://example.com:80
%   localhost:8080     -> http://localhost:8080
%   127.0.0.1          -> http://127.0.0.1:80
%   *.example.com      -> http://[...].example.com:80
%   http://            -> http://*:80
uri(Address) when is_list(Address); is_binary(Address) ->
    {ok, Pattern} = re:compile(
        <<
            "^"
            "((?<PROTOCOL>https?):\/\/)? # Optional protocol\n"
            "(?<HOST>[^/:]*?)            # Host (can be empty)\n"
            "(:(?<PORT>\\d+))?           # Optional port\n"
            "$"
        >>,
        [extended]
    ),
    PatternOpts = [anchored, {capture, all_names, binary}],
    {Host, Port, Scheme} =
        case re:run(Address, Pattern, PatternOpts) of
            {match, [H0, P0, S0]} ->
                {P1, S1} = uri_port_scheme(P0, S0),
                {uri_host(H0), P1, S1};
            _ ->
                error({invalid_listener_address, Address})
        end,
    URIString = <<Scheme/binary, "://", Host/binary, ":", Port/binary>>,
    ?LOG_WARNING(URIString),
    uri_string:parse(URIString).

uri_host(<<>>) -> <<"*">>;
uri_host(Host) -> Host.

uri_port_scheme(<<>>, <<>>) -> {<<"80">>, <<"http">>};
uri_port_scheme(<<>>, <<"https">>) -> {<<"443">>, <<"http">>};
uri_port_scheme(<<>>, <<"http">>) -> {<<"80">>, <<"http">>};
uri_port_scheme(<<"80">>, <<>>) -> {<<"80">>, <<"http">>};
uri_port_scheme(<<"443">>, <<>>) -> {<<"443">>, <<"https">>};
uri_port_scheme(Port, <<"http">>) -> {Port, <<"http">>};
uri_port_scheme(Port, <<"https">>) -> {Port, <<"https">>};
uri_port_scheme(Port, <<>>) -> {Port, <<"http">>};
uri_port_scheme(_Port, Scheme) -> error({invalid_listener_scheme, Scheme}).

route_host(<<"*">>) ->
    '_';
route_host(Host) ->
    binary:replace(Host, <<"*">>, <<"[...]">>).

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
