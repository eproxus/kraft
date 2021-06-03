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
        Handler:init({Req, State}, maps:get(bindings, Req, #{}))
    catch
        Class:Reason:Stacktrace ->
            Headers = #{<<"content-type">> => <<"text/html">>},
            Body = kraft_template:render(kraft, "500.html", #{
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
            Resp = cowboy_req:reply(500, Headers, Body, Req),
            {ok, Resp, State}
    end.

%--- Internal ------------------------------------------------------------------

format_stacktrace(Stacktrace) ->
    lists:map(fun format_stacktrace_entry/1, Stacktrace).

format_stacktrace_entry({M, F, Arity, _Loc}) when is_integer(Arity) ->
    #{module => M, function => F, arity => Arity};
format_stacktrace_entry({M, F, Args, _Loc}) ->
    #{module => M, function => F, args => [io_lib:format("~p", [A]) || A <- Args]}.

