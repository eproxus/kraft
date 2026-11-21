-module(kraft_json).

-moduledoc """
JSON encoding and decoding utilities for Kraft framework.

This module provides a thin wrapper around the `json` library with Kraft-specific
enhancements. It handles JSON encoding/decoding with automatic atom conversion
for object keys, making it easier to work with JSON data in Erlang applications.

## How It Works

The module extends the standard `json` library with Kraft-specific functionality:

1. **Decoding** - Converts JSON to Erlang terms with atom keys
2. **Encoding** - Converts Erlang terms to JSON
3. **Response formatting** - Creates HTTP responses with proper JSON headers
4. **Key conversion** - Automatically converts string keys to atoms

## Data Types

### Supported JSON Types

- **Objects** → Maps with atom keys
- **Arrays** → Lists
- **Strings** → Binaries
- **Numbers** → Integers or floats
- **Booleans** → Atoms `true` or `false`
- **Null** → Atom `null`

### Erlang Type Specifications

```erlang
-type body_json() :: {json, json:encode_value()}.
```

The `body_json()` type represents JSON data that can be used in Kraft responses.

## Error Handling

The module provides detailed error information:

```erlang
% Parse errors
{error, {json, Binary, {invalid_byte, Byte}}} ->
    % Invalid byte in JSON
    handle_invalid_byte(Byte);

{error, {json, Binary, {unexpected_sequence, Seq}}} ->
    % Unexpected character sequence
    handle_unexpected_sequence(Seq);

{error, {json, Binary, unexpected_end}} ->
    % JSON ended unexpectedly
    handle_unexpected_end();

{error, {json, Binary, {unexpected_result, Result}}} ->
    % Unexpected result from json:decode
    handle_unexpected_result(Result).
```

## Integration with Kraft

### WebSocket handlers

Check the `m:kraft_ws_json` module for an example of how to use this module
in a WebSocket handler.

### REST handlers
```erlang
% In kraft_rest behaviour
post(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    try
        {ok, Data} = kraft_json:decode(Body),
        Result = process_request(Data),
        kraft_json:response(Req2, Result)
    catch
        error:{json, _, Reason} ->
            Error = #{error => <<"Invalid JSON">>, reason => Reason},
            kraft_json:response(Req2, Error)
    end.
```

## See Also
- `json` - The underlying JSON library
- `m:kraft_ws_json` - WebSocket JSON behaviour
- `m:kraft_rest` - REST API handler
- `m:kraft_content` - Content type handling
- [JSON Specification](https://www.json.org/json-en.html)
""".

% API
-export([decode/1]).
-export([encode/1]).
-export([response/2]).

%--- Types ---------------------------------------------------------------------

-export_type([body_json/0]).

-type body_json() :: {json, json:encode_value()}.

%--- API -----------------------------------------------------------------------

-doc """
Decode JSON binary data to Erlang terms.

This function parses JSON data and converts it to Erlang terms. Object keys
are automatically converted from strings to atoms, making it easier to work
with the data in Erlang pattern matching.

## Key Conversion

String keys in JSON objects are automatically converted to atoms:

```json
{"first_name": "John", "last_name": "Doe"}
```

Becomes:
```erlang
#{first_name => <<"John">>, last_name => <<"Doe">>}
```

This enables pattern matching like:
```erlang
#{first_name := FirstName, last_name := LastName} = DecodedData.
```
""".
-spec decode(binary()) -> {ok, any()} | {error, {json, binary(), any()}}.
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

-doc """
Encode Erlang terms to JSON binary data.

This function converts Erlang terms to JSON format using the `json` library.
It's a direct wrapper around `json:encode/1` for consistency with the
Kraft framework.

## Supported Types

- **Maps** → JSON objects
- **Lists** → JSON arrays
- **Binaries** → JSON strings
- **Integers** → JSON numbers
- **Floats** → JSON numbers
- **Atoms** → JSON strings (except `true`, `false`, `null`)
- **Tuples** → JSON arrays


## Special Cases

- **Atom `true`** → JSON `true`
- **Atom `false`** → JSON `false`
- **Atom `null`** → JSON `null`
- **Other atoms** → JSON strings
- **Empty maps** → JSON `{}`
""".
-spec encode(any()) -> binary().
encode(Term) -> json:encode(Term).

-doc """
Create an HTTP response with JSON content.

This function formats an HTTP response with proper JSON headers and encoded
body. It automatically sets the Content-Type header to `application/json`
with UTF-8 charset.

## Headers Set

- **`content-type`** → `application/json; charset=utf8`

## Error Handling

The function will raise an error if the body cannot be encoded to JSON.
Always ensure your data structures are JSON-serializable.
""".
response(Conn0, Body) ->
    Conn1 = kraft_conn:response_headers(Conn0, #{
        <<"content-type">> => <<"application/json; charset=utf8">>
    }),
    kraft_conn:response_body(Conn1, encode(Body)).

%--- Internal ————————————------------------------------------------------------

-doc false.
object_push(Key, Value, Acc) ->
    [{kraft_util:attempt_atom(Key), Value} | Acc].
