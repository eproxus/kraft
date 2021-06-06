-module(ws).

-behaviour(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    kraft:start(#{port => 8092}, [
        {"/raw/ws", {ws, ws_raw}, #{}},
        {"/json/ws", {ws, ws_json}, #{}, #{type => json}},
        {"/jsonrpc/ws", {ws, ws_jsonrpc}, #{}, #{type => json_rpc}},
        {"/", kraft_static, #{}}
    ]),
    ws_sup:start_link().

stop(_State) ->
    ok.
