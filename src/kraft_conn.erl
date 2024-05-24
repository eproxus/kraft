-module(kraft_conn).

% API
-export([new/2]).
-export([await_body/1]).
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

-include_lib("kernel/include/logger.hrl").

%--- Types ---------------------------------------------------------------------

-export_type([conn/0]).
-export_type([params/0]).
-export_type([path/0]).
-export_type([status/0]).

-type path() :: list(binary()).
-type params() :: cowboy_router:bindings().
-type status() :: cowboy:http_status().

-type conn() :: #{
    '_type' => module(),
    meta => map(),
    adapter => {cowboy_req, cowboy_req:req()},
    headers => cowboy:http_headers(),
    path => path(),
    resp => #{
        status => kraft:status(),
        headers => kraft:headers(),
        body => kraft:body()
    },
    _ => _
}.

%--- API -----------------------------------------------------------------------

new(Req, Meta) ->
    Conn0 = #{adapter => {cowboy_req, Req}},
    Conn0#{
        '_type' => ?MODULE,
        adapter => {cowboy_req, Req},
        meta => Meta,
        method => raw(Conn0, method),
        path => kraft_util:split_path(raw(Conn0, path)),
        headers => raw(Conn0, headers),
        % FIXME: Should we really set default values here, or rather not have
        % the keys at all?
        resp => #{body => <<>>, headers => #{}}
    }.

await_body(#{adapter := {Module, Req}, meta := #{app := App}} = Conn0) ->
    case Module:has_body(Req) of
        true ->
            {Body, Req1} = await_body(App, Req, Module, <<>>),
            Conn0#{body => Body, adapter := {Module, Req1}};
        false ->
            Conn0#{body => undefined}
    end.

params(Conn0) -> raw(Conn0, bindings).

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
respond(#{resp := Resp, adapter := {Module, Req0}} = Conn0) ->
    case Resp of
        #{status := Status, body := Body, headers := Headers} ->
            Req1 = Module:reply(Status, Headers, Body, Req0),
            ?LOG_INFO(
                #{
                    method => maps:get(method, Conn0),
                    path => maps:get(path, Conn0),
                    status => Status,
                    size => byte_size(Body)
                },
                #{kraft_app => '_meta'(Conn0, app)}
            ),
            mapz:deep_merge(Conn0, #{
                resp => #{sent => true},
                adapter => {Module, Req1}
            });
        _ ->
            error(status_code_not_set)
    end.

-spec '_meta'(conn()) -> conn().
'_meta'(#{meta := Meta}) -> Meta.

'_meta'(Conn0, Key) when not is_list(Key) -> '_meta'(Conn0, [Key]);
'_meta'(Conn0, Path) -> mapz:deep_get([meta | Path], Conn0).

'_set_meta'(Conn0, Key, Value) when not is_list(Key) ->
    '_set_meta'(Conn0, [Key], Value);
'_set_meta'(Conn0, Path, Value) ->
    mapz:deep_put([meta | Path], Value, Conn0).

'_adapter'(#{adapter := Adapter}) -> Adapter.

%--- Internal ------------------------------------------------------------------

await_body(App, Req0, Module, Acc) ->
    case Module:read_body(Req0) of
        {ok, Data, Req1} ->
            Content = <<Acc/binary, Data/binary>>,
            ContentType = Module:parse_header(<<"content-type">>, Req0),
            Body = kraft_content:decode(App, ContentType, Content),
            {Body, Req1};
        {more, Data, Req1} ->
            await_body(App, Req1, Module, <<Acc/binary, Data/binary>>)
    end.

raw(#{adapter := {Module, Req}}, Function) -> apply(Module, Function, [Req]).
