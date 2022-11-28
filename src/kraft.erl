-module(kraft).

% API
-export([start/1]).
-export([start/2]).
-export([stop/1]).
-export([render/3]).

% Public API
-ignore_xref(start/1).
-ignore_xref(render/3).
-ignore_xref(start/2).
-ignore_xref(stop/0).
-ignore_xref(stop/1).

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

start(Opts, Routes) ->
    Owner = self(),
    kraft_dev:maybe_start(Opts),
    App = detect_app(Opts),
    {ok, Ref} = kraft_instance:start(#{
        app => App,
        routes => Routes,
        owner => Owner,
        opts => Opts
    }),
    Ref.

stop(Ref) -> kraft_instance:stop(Ref).

render(Conn, Template, Context) when is_list(Template) ->
    render(Conn, iolist_to_binary(Template), Context);
render(Conn, Template, Context) ->
    App = kraft_conn:'_meta'(Conn, app),
    Body = kraft_template:render(App, Template, Context),
    {kraft_template, #{<<"content-type">> => mime_type(Template)}, Body}.

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

mime_type(File) ->
    {Type, SubType, []} = cow_mimetypes:all(File),
    [Type, $/, SubType].
