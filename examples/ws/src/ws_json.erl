-module(ws_json).

-behaviour(kraft_ws_json).

% API
-export([init/1]).
-export([handle/2]).

%--- API -----------------------------------------------------------------------

init(State) ->
    {[{json, #{msg => <<"hello from kraft!">>}}], State}.

handle({json, #{msg := <<"Good bye!">>}}, State) ->
    {[{json, #{reply => <<"Bye!">>}}, close], State};
handle({json, JSON}, State) ->
    {[{json, #{echo => JSON}}], State}.
