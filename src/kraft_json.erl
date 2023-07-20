-module(kraft_json).

% API
-export([decode/1]).
-ignore_xref([{?MODULE, decode_stream, 1}]).
-export([decode_stream/1]).
-export([encode/1]).
-export([response/2]).

%--- Types ---------------------------------------------------------------------

-export_type([body_json/0]).

-type body_json() :: {json, jsx:json_term()}.

%--- API -----------------------------------------------------------------------

decode(Binary) -> jsx:decode(Binary, [{labels, attempt_atom}]).

decode_content(<<"application">>, <<"json">>, _Params, Value) ->
    decode(Value).

encode_content(JSON) -> {encode(JSON), <<"application/json; charset=utf8">>}.

decode_stream(Binary) ->
    jsx:decode(Binary, [{labels, attempt_atom}, stream, return_tail]).

encode(Term) -> jsx:encode(Term).

response(Conn0, Body) ->
    Conn1 = kraft_conn:response_headers(Conn0, #{
        <<"content-type">> => <<"application/json; charset=utf8">>
    }),
    kraft_conn:response_body(Conn1, encode(Body)).
