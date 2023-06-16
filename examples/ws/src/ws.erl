-module(ws).

-behaviour(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    Ref = kraft:start(#{port => 8092}, [
        {"/raw/ws", {ws, ws_raw}, #{}},
        {"/json/ws", {ws, ws_json}, #{}, #{type => json, ping => disabled}},
        {"/jsonrpc/ws", {ws, ws_jsonrpc}, #{}, #{
            type => json_rpc,
            ping => #{interval => 5_000}
        }},
        {"/", kraft_static, #{}}
    ]),
    {ok, Pid} = ws_sup:start_link(),
    {ok, Pid, Ref}.

stop(Ref) -> kraft:stop(Ref).
