-module(kraft).

% API
-export([start/1]).
-export([start/2]).
-export([stop/0]).
-export([render/3]).

% Public API
-ignore_xref({start, 1}).
-ignore_xref({render, 3}).
-ignore_xref({start, 2}).
-ignore_xref({stop, 0}).

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
    TransportOpts = [{port, Port}],
    ProtocolOpts = #{
        env => #{dispatch => {persistent_term, {kraft_dispatch, App}}},
        stream_handlers => [
            kraft_fallback_h,
            cowboy_compress_h,
            cowboy_stream_h
        ]
    },
    {ok, Pid} = cowboy:start_clear(Listener, TransportOpts, ProtocolOpts),
    link(Pid),

    ok.

stop() ->
    App =
        case application:get_application() of
            undefined -> error(cannot_determine_app);
            {ok, A} -> A
        end,
    cowboy:stop_listener(listener_name(App)).

render(App, Template, Context) when is_atom(App) ->
    Body = kraft_template:render(App, Template, Context),
    {kraft_template, #{<<"content-type">> => <<"text/html">>}, Body};
render(Conn, Template, Context) ->
    render(kraft_conn:meta(Conn, app), Template, Context).

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

route({Path, {ws, Handler}, State}, App) ->
    route({Path, {ws, Handler}, State, #{}}, App);
route({Path, {ws, Handler}, State, Opts}, App) ->
    [
        {Path, kraft_ws_util:module(Opts), #{
            opts => Opts,
            app => App,
            handler => Handler,
            state => State
        }}
    ];
route({Path, kraft_static, #{app := App}}, _App) ->
    static_routes(App, Path);
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
    Default = [{Path ++ "[...]", cowboy_static, {priv_dir, App, "web/static"}}],
    Static = kraft_file:path(App, static),
    Context = {Static, App, Path},
    StaticRoute = fun(File, Acc) -> static_route(File, Context, Acc) end,
    filelib:fold_files(Static, ".*", true, StaticRoute, Default).

static_route(File, {Static, App, Path}, Acc) ->
    Prefix = string:prefix(File, Static),
    PrivFile = {priv_file, App, ["web/static", Prefix]},
    Acc2 =
        case filename:basename(Prefix) of
            "index.html" ->
                [{filename:dirname(Prefix), cowboy_static, PrivFile} | Acc];
            _ ->
                Acc
        end,
    [{[Path, Prefix], cowboy_static, PrivFile} | Acc2].
