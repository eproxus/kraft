-module(ws_raw).

-behaviour(kraft_ws).

% API
-export([init/1]).
-export([handle/2]).

%--- API -----------------------------------------------------------------------

init(State) ->
    {[{text, <<"Hello from Kraft!">>}], State}.

handle({text, <<"Good bye!">>}, State) ->
    {[{text, <<"Bye!">>}, close], State};
handle({text, Text}, State) ->
    {[{text, [<<"You said: ">>, Text]}], State}.
