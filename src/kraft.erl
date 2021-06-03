-module(kraft).

% API
-export([start/1]).
-export([start/2]).
-export([stop/0]).
-export([render/3]).

%--- API -----------------------------------------------------------------------

start(Opts) -> start(Opts, []).

start(#{port := Port} = Opts, Routes) ->
    App = case maps:find(app, Opts) of
        error ->
            case application:get_application() of
                undefined -> error(could_not_determine_app);
                {ok, A} -> A
            end;
        {ok, A} ->
            A
    end,
    Dispatch = cowboy_router:compile([
        {'_', lists:flatten([
            routes(App, [{"/kraft", kraft_static, #{app => kraft}}|Routes])
        ])}
    ]),
    persistent_term:put({kraft_dispatch, App}, Dispatch),
    {ok, Pid} = cowboy:start_clear(listener_name(App),
        [{port, Port}],
        #{
            env => #{dispatch => {persistent_term, {kraft_dispatch, App}}},
            stream_handlers => [
                kraft_fallback_h,
                cowboy_compress_h,
                cowboy_stream_h
            ]
        }
    ),
    link(Pid),
    ok.

stop() ->
    App = case application:get_application() of
        undefined -> error(cannot_determine_app);
        {ok, A} ->A
    end,
    cowboy:stop_listener(listener_name(App)).

render({Req, #{app := App} = State}, Template, Context) ->
    Headers = #{<<"content-type">> => <<"text/html">>},
    Body = kraft_template:render(App, Template, Context),
    Resp = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Resp, State}.

%--- Internal ------------------------------------------------------------------

listener_name(App) ->
    list_to_atom("kraft_listener_" ++ atom_to_list(App)).

routes(App, Routes) ->
    lists:flatmap(fun(R) -> route(R, App)end, Routes).

route({Path, kraft_static, #{app := App}}, _App) ->
    static_routes(App, Path);
route({Path, kraft_static, _Opts}, App) ->
    static_routes(App, Path);
route({Path, Handler, Opts}, App) ->
    [{Path, kraft_controller, #{handler => Handler, app => App, opts => Opts}}].

static_routes(App, Path) ->
    Default = [{Path ++ "[...]", cowboy_static, {priv_dir, App, "web/static"}}],
    Static = kraft_file:path(App, static),
    filelib:fold_files(Static, <<".*">>, true, fun(File, Acc) ->
        Prefix = string:prefix(File, Static),
        PrivFile = {priv_file, App, ["web/static", Prefix]},
        Acc2 = case filename:basename(Prefix) of
            "index.html" ->
                [{filename:dirname(Prefix), cowboy_static, PrivFile}|Acc];
            _ ->
                Acc
        end,
        [{[Path, Prefix], cowboy_static, PrivFile}|Acc2]
    end, Default).
