-module(kraft_ws_util).

% API
-export([module/1]).
-export([callbacks/2]).
-export([handshake/2]).

%--- API -----------------------------------------------------------------------

module(Opts) ->
    module1(maps:merge(#{type => raw}, Opts)).

callbacks(Callbacks, #{handler := Handler} = State0) ->
    CheckCallback = fun(C, Cs) ->
        maps:put(C, lists:member(C, Handler:module_info(exports)), Cs)
    end,
    Exported = lists:foldl(CheckCallback, #{}, Callbacks),
    State0#{callbacks => Exported}.

handshake(Req, #{callbacks := #{{handshake, 3} := false}} = State) ->
    {cowboy_websocket, Req, State};
handshake(Req, #{handler := Handler, state := MState0} = State) ->
    Conn = kraft_conn:new(Req, State),
    case Handler:handshake({Req, MState0}, kraft_conn:params(Conn), MState0) of
        {reply, Code, Headers, Body} ->
            Resp = cowboy_req:reply(Code, Headers, Body, Req),
            {ok, Resp, State};
        {ok, MState1} ->
            {cowboy_websocket, Req, State#{state => MState1}}
    end.

%--- Internal ------------------------------------------------------------------

module1(#{type := raw}) -> kraft_ws;
module1(#{type := json}) -> kraft_ws_json;
module1(#{type := json_rpc}) -> kraft_ws_jsonrpc;
module1(#{type := Other}) -> error({invalid_kraft_ws_type, Other}).
