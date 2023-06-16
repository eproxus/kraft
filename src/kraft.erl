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
-export_type([params/0]).
-export_type([body/0]).
-export_type([response/0]).
-export_type([conn/0]).

-type status() :: non_neg_integer() | binary().
-type headers() :: #{atom() => iolist()}.
-type params() :: kraft_conn:params().
-type body() :: kraft_json:body_json() | cowboy_req:resp_body().
-type response() :: kraft_handler:response().
-type conn() :: kraft_conn:conn().

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

render(Conn0, Template, Context) ->
    kraft_template:render(Conn0, Template, Context).

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
