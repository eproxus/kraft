-module(kraft).

% API
-export([start/1]).
-export([start/2]).
-export([stop/0]).
-export([stop/1]).
-export([render/3]).

% Public API
-ignore_xref({start, 1}).
-ignore_xref({render, 3}).
-ignore_xref({start, 2}).
-ignore_xref({stop, 0}).
-ignore_xref({stop, 1}).

%--- Types ---------------------------------------------------------------------

-export_type([status/0]).
-export_type([headers/0]).
-export_type([body/0]).
-export_type([conn/0]).
-export_type([params/0]).

-type status() :: cowboy:http_status().
-type headers() :: cowboy:http_headers().
-type body() :: cowboy_req:resp_body().
-type conn() :: kraft_conn:conn().
-type params() :: map:map().

%--- API -----------------------------------------------------------------------

start(Opts) -> start(Opts, []).

start(#{port := Port} = Opts, Routes) ->
    App = detect_app(Opts),

    % Create routes
    InternalRoutes = {"/kraft", kraft_static, #{app => kraft}},
    AllRoutes = routes(App, [InternalRoutes | Routes]),
    Dispatch = cowboy_router:compile([{'_', lists:flatten(AllRoutes)}]),
    persistent_term:put({kraft_dispatch, App}, Dispatch),

    % Start Cowboy
    Listener = listener_name(App),
    ProtocolOpts = #{
        env => #{dispatch => {persistent_term, {kraft_dispatch, App}}},
        stream_handlers => [
            kraft_fallback_h,
            cowboy_compress_h,
            cowboy_stream_h
        ]
    },
    {ok, Pid} =
        case Opts of
            #{ssl_opts := SslOpts} ->
                TransportOpts = [{port, Port} | SslOpts],
                cowboy:start_tls(Listener, TransportOpts, ProtocolOpts);
            _ ->
                TransportOpts = [{port, Port}],
                cowboy:start_clear(Listener, TransportOpts, ProtocolOpts)
        end,
    link(Pid),

    ok.

stop() -> stop(detect_app(#{})).

stop(App) -> cowboy:stop_listener(listener_name(App)).

render(App, Template, Context) when is_atom(App) ->
    Body = kraft_template:render(App, Template, Context),
    {kraft_template, #{<<"content-type">> => <<"text/html">>}, Body};
render(Conn, Template, Context) ->
    render(kraft_conn:'_meta'(Conn, app), Template, Context).

%--- Internal ------------------------------------------------------------------

detect_app(Opts) ->
    case maps:find(app, Opts) of
        error ->
            case application:get_application() of
                undefined -> error(could_not_determine_app);
                {ok, A} -> A
            end;
        {ok, A} ->
            A
    end.

listener_name(App) ->
    list_to_atom("kraft_listener_" ++ atom_to_list(App)).

routes(App, Routes) ->
    lists:flatmap(fun(R) -> route(R, App) end, Routes).

route({Path, {ws, Handler}, MState}, App) ->
    route({Path, {ws, Handler}, MState, #{}}, App);
route({Path, {ws, Handler}, MState, Opts}, App) ->
    {Module, State} = kraft_ws_util:setup(Opts, App, Handler, MState),
    [{Path, Module, State}];
route({Path, kraft_static, #{file := File} = State}, App) ->
    StaticFile = filename:join("web/static/", File),
    [{Path, cowboy_static, {priv_file, maps:get(app, State, App), StaticFile}}];
route({Path, kraft_static, State}, App) ->
    static_routes(maps:get(app, State, App), Path);
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
