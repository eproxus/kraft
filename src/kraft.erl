-module(kraft).

% API
-export([start/1]).
-export([start/2]).
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
    Static = static_routes(App),
    Dispatch = cowboy_router:compile([
        {'_', lists:flatten([
            {"/assets/kraft/kraft.css", cowboy_static, {priv_file, kraft, "web/static/assets/styles/kraft.css"}},
            [{Path, kraft_handler, #{handler => Handler, app => App}} || {Path, Handler} <- Routes],
            Static,
            {"/[...]", kraft_fallback_handler, []}
        ])}
    ]),
    persistent_term:put({kraft_dispatch, App}, Dispatch),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, Port}],
        #{env => #{dispatch => {persistent_term, {kraft_dispatch, App}}}}
    ),
    ok.

render({Req, #{app := App} = State}, Template, Context) ->
    Headers = #{<<"content-type">> => <<"text/html">>},
    Body = kraft_template:render(App, Template, Context),
    Resp = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Resp, State}.

%--- Internal ------------------------------------------------------------------

static_routes(App) ->
    Static = kraft_file:path(App, static),
    filelib:fold_files(Static, <<".*">>, true, fun(File, Acc) ->
        Path = string:prefix(File, Static),
        PrivFile = {priv_file, App, ["web/static", Path]},
        Acc2 = case filename:basename(Path) of
            "index.html" -> [{filename:dirname(Path), cowboy_static, PrivFile}|Acc];
            _ -> Acc
        end,
        [{["/", Path], cowboy_static, PrivFile}|Acc2]
    end, []).
