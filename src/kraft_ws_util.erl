-module(kraft_ws_util).

% API
-export([module/1]).
-export([callbacks/2]).

%--- API -----------------------------------------------------------------------

module(Opts) ->
    module1(maps:merge(#{type => raw}, Opts)).

callbacks(Module, Callbacks) ->
    lists:foldl(fun(C, Cs) ->
        maps:put(C, lists:member(C, Module:module_info(exports)), Cs)
    end, #{}, Callbacks).

%--- Internal ------------------------------------------------------------------

module1(#{type := raw}) ->
    kraft_ws;
module1(#{type := json}) ->
    kraft_ws_json;
module1(#{type := json_rpc}) ->
    kraft_ws_jsonrpc;
module1(#{type := Other}) ->
    error({invalid_kraft_ws_type, Other}).
