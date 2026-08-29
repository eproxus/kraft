-module(kraft_jsonrpc).

-moduledoc """
JSON-RPC 2.0 protocol implementation for Kraft framework.

This module provides a complete implementation of the JSON-RPC 2.0 specification,
handling encoding, decoding, validation, and error formatting for JSON-RPC messages.
It's designed to work seamlessly with Kraft's WebSocket and HTTP handlers.

## Features

* **Full JSON-RPC 2.0 compliance** - Implements the complete specification
* **Batch processing** - Handles both single messages and batch requests
* **Automatic validation** - Validates message structure and format
* **Error handling** - Standard JSON-RPC error codes and messages
* **Type safety** - Strong Erlang type specifications
* **Easy integration** - Works with any JSON-RPC client or server

## JSON-RPC 2.0 Protocol

To know more about the JSON-RPC 2.0 protocol, please refer to the
[JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification).

### Error Codes

Check the function `kraft_jsonrpc:format_error/1` for more information about
the error codes and their meaning.

## Integration with Kraft

You can use this module in two ways:

- **WebSocket JSON-RPC Handler** - For WebSocket connections
- **HTTP JSON-RPC Endpoint** - For HTTP connections

### WebSocket JSON-RPC Handler

Check the `m:kraft_ws_jsonrpc` module for an example of how to use this module
in a WebSocket JSON-RPC handler.

### HTTP JSON-RPC Endpoint

For HTTP JSON-RPC endpoints, you can use the `m:kraft_rest` module.

Example:
```erlang
-module(jsonrpc_http_handler).
-behaviour(kraft_rest).

export([init/2, post/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

post(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case kraft_jsonrpc:decode(Body) of
        {single, {call, Method, Params, Id}} ->
            Result = execute_method(Method, Params),
            Response = kraft_jsonrpc:encode({result, Result, Id}),
            {Response, Req2, State};
        {single, {error, Code, Message, Data, Id}} ->
            Response = kraft_jsonrpc:encode({error, Code, Message, Data, Id}),
            {Response, Req2, State}
    end.
```

## Message Validation

The module automatically validates all incoming messages:

- **JSON-RPC version** - Must be exactly "2.0"
- **Method names** - Must be atoms or binaries
- **Parameters** - Must be maps or lists
- **Message structure** - Must contain required fields

## Batch Processing

JSON-RPC 2.0 supports batch requests for multiple method calls:

```json
[
  {"jsonrpc": "2.0", "method": "getUser", "params": {"id": 1}, "id": 1},
  {"jsonrpc": "2.0", "method": "getUser", "params": {"id": 2}, "id": 2}
]
```

The module automatically detects batch requests and processes them accordingly.

## See Also
- `m:kraft_ws_jsonrpc` - WebSocket JSON-RPC behaviour
- `m:kraft_json` - JSON encoding/decoding utilities
- `m:kraft_ws` - Base WebSocket behaviour
- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)
""".

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

-doc """
This type represents a JSON-RPC message.

- **Method Calls (expecting response)**
    - **`{call, Method, Params, ID}`** - Method call with parameters
        - `Method` is an atom (converted from string)
        - `Params` are the method parameters (map or list)
        - `ID` is the request identifier for response matching

- **Notifications (no response expected)**:
    - **`{notification, Method, Params}`** - Method call without response
        - `Method` is an atom (converted from string)
        - `Params` are the method parameters (map or list)

More information about this type can be found in the
[JSON-RPC 2.0 Specification Request Object](https://www.jsonrpc.org/specification#request_object).

""".
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

-doc """
Decode JSON-RPC messages from binary JSON data.

This function parses JSON-RPC 2.0 messages and returns them in a structured
Erlang format. It handles both single messages and batch requests, automatically
validating the JSON-RPC protocol compliance.

## Examples

```erlang
% Decode method call
{ok, {single, {call, getUser, #{<<"id">> => 123}, 1}} = 
    kraft_jsonrpc:decode(<<"{\"jsonrpc\":\"2.0\",\"method\":\"getUser\",\"params\":{\"id\":123},\"id\":1}">>).

% Decode notification
{ok, {single, {notification, logEvent, #{<<"event">> => <<"login">>}}} =
    kraft_jsonrpc:decode(<<"{\"jsonrpc\":\"2.0\",\"method\":\"logEvent\",\"params\":{\"event\":\"login\"}}">>).

% Decode batch request
{ok, {batch, [
    {call, getUser, #{<<"id">> => 1}, 1},
    {call, getUser, #{<<"id">> => 2}, 2}
]}} = kraft_jsonrpc:decode(BatchJson).

% Handle parse errors
{ok, {single, {internal_error, parse_error, null}} = 
    kraft_jsonrpc:decode(<<"invalid json">>).
```

## Error Handling

The function handles these error cases:

- **Parse errors** - Invalid JSON syntax
- **Invalid requests** - Malformed JSON-RPC messages
- **Missing fields** - Required JSON-RPC fields not present
- **Type mismatches** - Invalid data types for fields

## Validation

Each message is automatically validated for:

- JSON-RPC version must be "2.0"
- Method names must be atoms or binaries
- Parameters must be maps or lists
- Required fields must be present
""".
-spec decode(binary()) ->
    {ok, {single, message()} | {batch, [message()]}}
    | {error, {json, binary(), any()}}.
decode(Binary) ->
    try kraft_json:decode(Binary) of
        [] ->
            {single, {internal_error, invalid_request, null}};
        Messages when is_list(Messages) ->
            {batch, [unpack(M) || M <- Messages]};
        Message when is_map(Message) ->
            {single, unpack(Message)};
        _Other ->
            {single, {internal_error, invalid_request, id(_Other)}}
    catch
        error:{json, _JSON, _Reason} ->
            {single, {internal_error, parse_error, null}}
    end.

-doc """
Encode JSON-RPC messages to binary JSON data.

This function converts Erlang JSON-RPC message tuples to JSON binary format
according to the JSON-RPC 2.0 specification. It handles both single messages
and batch responses.

## Examples

```erlang
% Encode single message
kraft_jsonrpc:encode({call, Method, Params, ID}).

% Response:
% {"jsonrpc": "2.0", "method": "getUser", "params": {"id": 1}, "id": 1}

% Encode batch messages
kraft_jsonrpc:encode([{call, Method, Params, ID}, {call, Method, Params, ID}]).

% Response:
%[
%    {"jsonrpc": "2.0", "method": "getUser", "params": {"id": 1}, "id": 1},
%    {"jsonrpc": "2.0", "method": "getUser", "params": {"id": 2}, "id": 2}
%]
```

## Batch Processing

When encoding a list of messages, each message is individually packed and
then combined into a JSON array. This ensures proper JSON-RPC 2.0 compliance
for batch responses.

## Error Handling

The function automatically handles standard error types by calling `format_error/1`
when you pass `{error, ErrorType, ID}` format.
""".
-spec encode([message()] | message() | any()) -> binary().
encode(Messages) when is_list(Messages) ->
    kraft_json:encode([pack(M) || M <- Messages]);
encode(Message) ->
    kraft_json:encode(pack(Message)).

-doc """
Format standard JSON-RPC 2.0 error responses.

This function converts internal error types to standard JSON-RPC 2.0 error
responses with proper error codes and messages according to the specification.

Check the
[JSON-RPC 2.0 Specification Error Object](https://www.jsonrpc.org/specification#error_object)
for more information about the error codes and their meaning.

## Error Types

The function handles these standard JSON-RPC 2.0 error types:

- **`parse_error`** → Code -32700, "Parse error"
- **`invalid_request`** → Code -32600, "Invalid Request"
- **`method_not_found`** → Code -32601, "Method not found"
- **`invalid_params`** → Code -32602, "Invalid params"
- **`internal_error`** → Code -32603, "Internal error"
""".
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

-doc false.
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

-doc false.
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
pack({error, Type, ID}) ->
    pack(format_error({internal_error, Type, ID}));
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

-doc false.
id(Object) when is_map(Object) -> maps:get(id, Object, undefined);
id(_Object) -> undefined.
