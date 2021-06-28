-module(rest).

-behaviour(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_StartType, _StartArgs) ->
    kraft:start(#{port => 8094}, [
        {"/api", {cowboy, rest_api}, #{}},
        {"/", kraft_static, #{}}
    ]),
    {ok, self()}.

stop(_State) ->
    ok.
