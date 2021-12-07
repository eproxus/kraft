-module(ws_json).

-behaviour(kraft_ws_json).

% API
-export([init/2]).
-export([handle/2]).

%--- API -----------------------------------------------------------------------

init(_Conn, State) ->
    {[{json, #{msg => <<"hello from kraft!">>}}], State}.

handle({json, #{msg := <<"Good bye!">>}}, State) ->
    {[{json, #{reply => <<"Bye!">>}}, {close, <<"See you soon!">>}], State};
handle({json, JSON}, State) ->
    {[{json, #{echo => JSON}}], State}.
