-module(kraft_jsonrpc).

% API
-export([decode/1]).
-export([encode/1]).
-export([format_error/1]).

%--- Types ---------------------------------------------------------------------

-type method() :: atom() | binary().
-type params() :: [any()] | map().
-type id() :: undefined | any().
-type result() :: any().
-type code() :: integer().
-type error_message() :: binary().
-type data() :: any().
-type message() ::
    {call, method(), params(), id()}
    | {notification, method(), params()}
    | {result, result(), id()}
    | {error, code(), error_message(), data(), id()}.

-export_type([message/0]).

%--- Macros --------------------------------------------------------------------

-define(V, jsonrpc => <<"2.0">>).

-define(is_valid(Message),
    (map_get(jsonrpc, Message) == <<"2.0">>)
).
-define(is_method(Method),
    (is_atom(Method) orelse is_binary(Method))
).
-define(is_message(Method),
    (is_atom(Method) orelse is_binary(Method))
).
-define(is_params(Params),
    (is_map(Params) orelse is_list(Params))
).

%--- API ----------------------------------------------------------------------

decode(Binary) ->
    case decode_json(Binary) of
        {ok, []} ->
            {single, {internal_error, invalid_request, null}};
        {ok, Messages} when is_list(Messages) ->
            {batch, [unpack(M) || M <- Messages]};
        {ok, Message} when is_map(Message) ->
            {single, unpack(Message)};
        {ok, _Other} ->
            {single, {internal_error, invalid_request, id(_Other)}};
        {error, parse_error} ->
            {single, {internal_error, parse_error, null}}
    end.

encode(Messages) when is_list(Messages) ->
    encode_json([pack(M) || M <- Messages]);
encode(Message) ->
    encode_json(pack(Message)).

format_error({internal_error, parse_error, ID}) ->
    {error, -32700, <<"Parse error">>, undefined, ID};
format_error({internal_error, invalid_request, ID}) ->
    {error, -32600, <<"Invalid Request">>, undefined, ID};
format_error({internal_error, method_not_found, ID}) ->
    {error, -32601, <<"Method not found">>, undefined, ID};
format_error({internal_error, invalid_params, ID}) ->
    {error, -32602, <<"Invalid params">>, undefined, ID};
format_error({internal_error, internal_error, ID}) ->
    {error, -32603, <<"Internal error">>, undefined, ID}.

%--- Internal -----------------------------------------------------------------

decode_json(Binary) ->
    try
        {ok, jsone:decode(Binary, [{keys, attempt_atom}])}
    catch
        error:badarg:ST ->
            case ST of
                [Call | _] when element(1, Call) == jsone_decode ->
                    {error, parse_error};
                _ ->
                    erlang:raise(error, badarg, ST)
            end
    end.

unpack(#{method := Method, params := Params, id := ID} = M) when
    ?is_valid(M), ?is_method(Method), ?is_params(Params)
->
    {call, Method, Params, ID};
unpack(#{method := Method, id := ID} = M) when
    ?is_valid(M), ?is_method(Method)
->
    {call, Method, undefined, ID};
unpack(#{method := Method, params := Params} = M) when
    ?is_valid(M), ?is_method(Method), ?is_params(Params)
->
    {notification, Method, Params};
unpack(#{method := Method} = M) when
    ?is_valid(M), ?is_method(Method)
->
    {notification, Method, undefined};
unpack(#{method := Method, params := _Params, id := ID} = M) when
    ?is_valid(M), ?is_method(Method)
->
    {internal_error, invalid_params, ID};
unpack(#{result := Result, id := ID} = M) when
    ?is_valid(M)
->
    {result, Result, ID};
unpack(
    #{error := #{code := Code, message := Message, data := Data}, id := ID} = M
) when
    ?is_valid(M)
->
    {error, Code, Message, Data, ID};
unpack(#{error := #{code := Code, message := Message}, id := ID} = M) when
    ?is_valid(M)
->
    {error, Code, Message, undefined, ID};
unpack(M) ->
    {internal_error, invalid_request, id(M)}.

pack({call, Method, undefined, ID}) ->
    #{?V, method => Method, id => ID};
pack({call, Method, Params, ID}) ->
    #{?V, method => Method, params => Params, id => ID};
pack({notification, Method, undefined}) ->
    #{?V, method => Method};
pack({notification, Method, Params}) ->
    #{?V, method => Method, params => Params};
pack({result, Result, ID}) ->
    #{?V, result => Result, id => ID};
pack({error, Code, Message, undefined, undefined}) ->
    #{?V, error => #{code => Code, message => Message}, id => null};
pack({error, Code, Message, undefined, ID}) ->
    #{?V, error => #{code => Code, message => Message}, id => ID};
pack({error, Code, Message, Data, undefined}) ->
    #{
        ?V,
        error => #{code => Code, message => Message, data => Data, id => null}
    };
pack({error, Code, Message, Data, ID}) ->
    #{?V, error => #{code => Code, message => Message, data => Data}, id => ID}.

encode_json(JSON) -> jsone:encode(JSON).

id(Object) when is_map(Object) -> maps:get(id, Object, undefined);
id(_Object) -> undefined.
