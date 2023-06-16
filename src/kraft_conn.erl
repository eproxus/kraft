-module(kraft_conn).

% API
-export([new/2]).
-export([method/1]).
-export([path/1]).
-export([params/1]).
-export([response/4]).
-export([response_status/2]).
-export([response_headers/1]).
-export([response_headers/2]).
-export([response_body/1]).
-export([response_body/2]).
-export([respond/1]).
-export(['_meta'/1]).
-export(['_meta'/2]).
-export(['_set_meta'/3]).
-export(['_adapter'/1]).

%--- Types ---------------------------------------------------------------------

-export_type([conn/0]).
-export_type([params/0]).

-type params() :: cowboy_router:bindings().

-type conn() :: #{
    '_type' => module(),
    meta => map(),
    adapter => {cowboy_req, cowboy_req:req()},
    resp => #{
        status => kraft:status(),
        headers => kraft:headers(),
        body => kraft:body()
    },
    _ => _
}.

%--- API -----------------------------------------------------------------------

new(Req, Meta) ->
    #{
        '_type' => ?MODULE,
        adapter => {cowboy_req, Req},
        meta => Meta,
        resp => #{body => <<>>, headers => #{}}
    }.

method(#{adapter := {Module, Req}} = Conn0) -> {Module:method(Req), Conn0}.

path(#{request := #{path := Path}} = Conn0) ->
    {Path, Conn0};
path(#{adapter := {Module, Req}} = Conn0) ->
    Path = kraft_util:split_path(Module:path(Req)),
    {Path, mapz:deep_put([request, path], Path, Conn0)}.


params(#{adapter := {Module, Req}} = Conn0) -> {Module:bindings(Req), Conn0}.

response(Conn0, Status, Headers, Body) ->
    mapz:deep_merge(Conn0, #{
        resp => #{
            status => Status,
            headers => Headers,
            body => Body
        }
    }).

response_status(Conn0, Status) when is_integer(Status) ->
    mapz:deep_put([resp, status], Status, Conn0).

response_headers(#{resp := #{headers := Headers}}) -> Headers.

response_headers(Conn0, Headers) when is_map(Headers) ->
    mapz:deep_merge(Conn0, #{resp => #{headers => Headers}}).

response_body(#{resp := #{body := Body}}) -> Body.

response_body(#{resp := #{sent := true}}, _Body) ->
    error(response_already_sent);
response_body(Conn0, Body) ->
    mapz:deep_put([resp, body], Body, Conn0).

respond(#{resp := #{sent := true}}) ->
    error(response_already_sent);
respond(
    #{
        resp := #{status := Status, body := Body, headers := Headers},
        adapter := {Module, Req0}
    } = Conn0
) ->
    Req1 = Module:reply(Status, Headers, Body, Req0),
    mapz:deep_merge(Conn0, #{
        resp => #{sent => true},
        adapter => {Module, Req1}
    });
respond(_Conn0) ->
    error(status_code_not_set).

-spec '_meta'(conn()) -> conn().
'_meta'(#{meta := Meta}) -> Meta.

'_meta'(Conn0, Key) when not is_list(Key) -> '_meta'(Conn0, [Key]);
'_meta'(Conn0, Path) -> mapz:deep_get([meta | Path], Conn0).

'_set_meta'(Conn0, Key, Value) when not is_list(Key) ->
    '_set_meta'(Conn0, [Key], Value);
'_set_meta'(Conn0, Path, Value) ->
    mapz:deep_put([meta | Path], Value, Conn0).

'_adapter'(#{adapter := Adapter}) -> Adapter.
