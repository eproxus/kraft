-module(ws_raw).

-behaviour(kraft_ws).

% API
-export([init/1]).
-export([handle/2]).

%--- API -----------------------------------------------------------------------

init(State) ->
    {[{text, "Hello from Kraft!"}], State}.

handle({text, Text}, State) ->
    {[{text, ["You said: ", Text]}], State};
handle(_Frame, State) ->
    {[], State}.
