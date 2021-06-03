-module(kraft_controller).

-behaviour(cowboy_handler).

% API
-export([init/2]).

%--- Types ---------------------------------------------------------------------

-type conn() :: {cowboy_req:req(), any()}.
-type params() :: map:map().

-callback init(conn(), params()) -> ok.

%--- API -----------------------------------------------------------------------

init(#{path := Path, method := Method} = Req, #{handler := Handler} = State) ->
    try
        {Code, Headers, Body} = case Handler:init({Req, State}, maps:get(bindings, Req, #{})) of
            {C, H, {kraft_template, TH, B}} -> {C, maps:merge(TH, H), B};
            {C, H, B} when is_binary(B) -> {C, H, B}
        end,
        Resp = cowboy_req:reply(Code, Headers, Body, Req),
        {ok, Resp, State}
    catch
        Class:Reason:Stacktrace ->
            EHeaders = #{<<"content-type">> => <<"text/html">>},
            EBody = kraft_template:render(kraft, "500.html", #{
                req => #{
                    url => Path,
                    method => Method
                },
                exception => #{
                    class => Class,
                    reason => io_lib:format("~p", [Reason]),
                    stacktrace => format_stacktrace(Stacktrace)
                }
            }),
            EResp = cowboy_req:reply(500, EHeaders, EBody, Req),
            {ok, EResp, State}
    end.

%--- Internal ------------------------------------------------------------------

format_stacktrace(Stacktrace) ->
    lists:map(fun format_stacktrace_entry/1, Stacktrace).

format_stacktrace_entry({M, F, Arity, _Loc}) when is_integer(Arity) ->
    #{module => M, function => F, arity => Arity};
format_stacktrace_entry({M, F, Args, _Loc}) ->
    #{module => M, function => F, args => [io_lib:format("~p", [A]) || A <- Args]}.

