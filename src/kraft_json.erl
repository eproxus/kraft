-module(kraft_json).

% API
-export([decode/1]).
-export([encode/1]).
-export([response/2]).

%--- Types ---------------------------------------------------------------------

-export_type([body_json/0]).

-type body_json() :: {json, json:encode_value()}.

%--- API -----------------------------------------------------------------------

decode(Binary) ->
    try json:decode(Binary, ok, #{object_push => fun object_push/3}) of
        {Result, ok, <<>>} -> Result;
        Result -> error({json, Binary, {unexpected_result, Result}})
    catch
        error:{invalid_byte, _} = Reason ->
            error({json, Binary, Reason});
        error:{unexpected_sequence, _} = Reason ->
            error({json, Binary, Reason});
        error:unexpected_end = Reason ->
            error({json, Binary, Reason})
    end.

encode(Term) -> json:encode(Term).

response(Conn0, Body) ->
    Conn1 = kraft_conn:response_headers(Conn0, #{
        <<"content-type">> => <<"application/json; charset=utf8">>
    }),
    kraft_conn:response_body(Conn1, encode(Body)).

%--- Internal ————————————------------------------------------------------------

object_push(Key, Value, Acc) ->
    [{kraft_util:attempt_atom(Key), Value} | Acc].
