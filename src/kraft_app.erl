%%%-------------------------------------------------------------------
%% @doc kraft public API
%% @end
%%%-------------------------------------------------------------------

-module(kraft_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kraft_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
