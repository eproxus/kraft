-module(kraft_conn).

% API
-export([new/2]).
-export([params/1]).
-export([merge_resp_headers/2]).
-export([send_resp/3]).
-export(['_meta'/1]).
-export(['_meta'/2]).
-export(['_set_meta'/3]).
-export(['_adapter'/1]).

%--- Types ---------------------------------------------------------------------

-export_type([conn/0]).

-type conn() :: {cowboy_req:req(), any()}.

%--- API -----------------------------------------------------------------------

new(Req, Meta) -> #{adapter => {cowboy_req, Req}, meta => Meta}.

params(#{adapter := {Module, Req}}) -> Module:bindings(Req).

resp(Conn, Status, Body) ->
    Conn#{resp_status => Status, resp_body => Body}.

merge_resp_headers(Conn, Headers) ->
    Existing = maps:get(resp_headers, Conn, #{}),
    Conn#{resp_headers => maps:merge(Existing, Headers)}.

send_resp(#{resp_sent := true}) ->
    error(response_already_sent);
send_resp(#{resp_status := Status, resp_body := Body} = Conn) ->
    #{adapter := {Module, Req0}} = Conn,
    Req1 = Module:reply(Status, maps:get(resp_headers, Conn, #{}), Body, Req0),
    Conn#{resp_sent => true, adapter => {Module, Req1}}.

send_resp(Conn, Status, Body) ->
    send_resp(resp(Conn, Status, Body)).

'_meta'(#{meta := Meta}) -> Meta.

'_meta'(Conn, Key) when not is_list(Key) ->
    '_meta'(Conn, [Key]);
'_meta'(Conn, Path) ->
    mapz:deep_get([meta | Path], Conn).

'_set_meta'(Conn, Key, Value) when not is_list(Key) ->
    '_set_meta'(Conn, [Key], Value);
'_set_meta'(Conn, Path, Value) ->
    mapz:deep_put([meta | Path], Value, Conn).

'_adapter'(#{adapter := Adapter}) -> Adapter.
