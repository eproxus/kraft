-module(ws_jsonrpc).

-behaviour(kraft_ws_jsonrpc).

% API
-export([init/1]).
-export([message/2]).
-export([info/2]).

%--- API -----------------------------------------------------------------------

init(State) ->
    State.

% Calls
message({call, subtract, [A, B], ID}, State) ->
    {[{result, A - B, ID}], State};
message({call, subtract, #{minuend := A, subtrahend := B}, ID}, State) ->
    {[{result, A - B, ID}], State};
message({call, foobar, _Params, ID}, State) ->
    {[{error, method_not_found, ID}], State};
message({call, sum, Params, ID}, State) when is_list(Params) ->
    {[{result, lists:sum(Params), ID}], State};
message({call, get_data, undefined, ID}, State) ->
    {[{result, [<<"hello">>, 5], ID}], State};
message({call, async, undefined, ID}, State) ->
    self() ! {async_result, ID},
    {[], State};
message({call, goodbye, #{time := Time}, ID}, State) when is_integer(Time) ->
    erlang:send_after(Time, self(), {goodbye, ID}),
    {[], State};
% Notifications
message({notification, update, _Params}, State) ->
    {[], State};
message({notification, foobar, undefined}, State) ->
    {[], State};
message({notification, notify_hello, _}, State) ->
    {[], State}.

info({async_result, ID}, State) ->
    {[{result, <<"async result">>, ID}], State};
info({goodbye, ID}, State) ->
    {[{result, bye, ID}, close], State}.
