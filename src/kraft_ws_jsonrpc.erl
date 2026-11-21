-module(kraft_ws_jsonrpc).

-moduledoc """
JSON-RPC 2.0 WebSocket behaviour for Kraft framework.

This module defines the behaviour for implementing JSON-RPC 2.0 over WebSocket
connections in Kraft. It automatically handles JSON-RPC protocol parsing,
validation, and response formatting, making it perfect for building RPC APIs
over WebSocket connections.

Check the module `m:kraft_jsonrpc` for more information about the JSON-RPC 2.0
protocol.

## How It Works

The `m:kraft_ws_jsonrpc` behaviour extends `m:kraft_ws` and use 
`m:kraft_jsonrpc` to handle the JSON-RPC 2.0 protocol:

1. **Receives WebSocket text frames** containing JSON-RPC messages
2. **Parses JSON-RPC protocol** using `m:kraft_jsonrpc`
3. **Routes method calls** to your implementation of the `c:message/2` callback
4. **Handles batch requests** automatically
5. **Formats responses** according to JSON-RPC 2.0 specification
6. **Sends responses** as WebSocket text frames

```
Client JSON-RPC → WebSocket Text Frame → kraft_ws_jsonrpc → Parse → Your Handler
Your Handler → Response → kraft_ws_jsonrpc → Format → WebSocket Text Frame → Client
```

## Callbacks

### Required Callbacks

- **`c:kraft_ws:init/2`** - Initialize the JSON-RPC WebSocket connection
- **`c:kraft_ws_jsonrpc:message/2`** - Handle incoming JSON-RPC method `calls` and `notifications`

### Optional Callbacks

- **`handshake/3`** - Custom handshake logic (authentication, validation)
- **`info/2`** - Handle Erlang messages sent to the WebSocket process
- **`terminate/3`** - Cleanup when connection closes

## Usage

```erlang
-module(my_jsonrpc_handler).
-behaviour(kraft_ws_jsonrpc).

-export([init/2, message/2]).

init(_Conn, State) ->
    % Initialize your handler state
    State#{counter => 0}.

message({call, increment, #{<<"amount">> := Amount}, ID}, #{counter := Counter} = State) ->
    NewCounter = Counter + Amount,
    {[{result, NewCounter, ID}], State#{counter => NewCounter}};

message({call, get_counter, _Params, ID}, #{counter := Counter} = State) ->
    {[{result, Counter, ID}], State};

message({call, divide, #{<<"a">> := A, <<"b">> := B}, ID}, State) ->
    case B of
        0 -> 
            {[{error, -32602, <<"Division by zero">>, undefined, ID}], State};
        _ -> 
            Result = A / B,
            {[{result, Result, ID}], State}
    end;

message({notification, log_event, #{<<"event">> := Event}, State) ->
    % Handle notification (no response needed)
    io:format("Event logged: ~p~n", [Event]),
    {[], State};

message({call, unknown_method, _Params, ID}, State) ->
    % Return standard error
    {[{error, method_not_found, ID}], State}.
```

Configure JSON-RPC WebSocket routes in your Kraft application:

```erlang
Routes = [
    {"/ws/rpc", {ws, my_jsonrpc_handler}, #{}, #{
        type => jsonrpc,
        ping => #{interval => 30000}
    }}
].

kraft:start(#{port => 8080}, Routes).
```

## Batch Processing

The behaviour automatically handles JSON-RPC batch requests thanks to
`kraft_jsonrpc:decode/1`.

Each method call is processed individually and responses are collected into a
batch response.


## Error Handling

### Automatic Error Handling
- **Parse errors** - Invalid JSON automatically handled
- **Invalid requests** - Malformed JSON-RPC messages
- **Method not found** - Automatic error response for unknown methods

### Custom Error Handling
```erlang
message({call, validate_user, #{<<"email">> := Email}, ID}, State) ->
    case validate_email(Email) of
        {ok, User} -> {[{result, User, ID}], State};
        {error, Reason} -> 
            {[{error, -32602, <<"Invalid email">>, Reason, ID}], State}
    end.
```

## JSON conversion

Check the `m:kraft_jsonrpc` module for more information about the JSON conversion.

## See Also
- `m:kraft_ws` - Base WebSocket behaviour
- `m:kraft_ws_util` - Core WebSocket utility module
- `m:kraft_jsonrpc` - JSON-RPC 2.0 protocol implementation
- `m:kraft_json` - JSON encoding/decoding utilities
- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)
""".

% Callbacks
-export([init/2]).
-ignore_xref(init/2).
-export([handle/2]).
-ignore_xref(handle/2).
-export([info/2]).
-ignore_xref(info/2).
-export([terminate/3]).
-ignore_xref(terminate/3).

%--- Includes ------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

%--- Types ---------------------------------------------------------------------

-type state() :: any().

%--- Callbacks -----------------------------------------------------------------

-doc """
Optional callback for custom WebSocket handshake logic.

This callback is called during the WebSocket handshake process and allows you
to implement custom authentication, validation, or response logic for JSON-RPC
connections.

Check the `c:kraft_ws:handshake/3` callback for more information.
""".
-optional_callbacks([handshake/3]).
-callback handshake(kraft:conn(), kraft_conn:params(), state()) ->
    {reply, kraft:status(), kraft:headers(), kraft:body()}
    | {ok, state()}
    | {ok, kraft:headers(), state()}.

-doc """
Required callback for initializing the JSON-RPC WebSocket connection.

This callback is called when a WebSocket connection is established. It's the
perfect place to set up connection state, initialize resources, or perform
any initialization logic for your JSON-RPC handler.

Check the `c:kraft_ws:init/2` callback for more information.
""".
-callback init(kraft:conn(), state()) -> state().

-doc """
Required callback for handling JSON-RPC method calls and notifications.

This callback is called for every JSON-RPC message received from the client.
It's the core of your JSON-RPC implementation where you handle method calls
and return appropriate responses.

## Message Types

Check the `m:kraft_jsonrpc` module for more information about the JSON-RPC 2.0
message types and their meaning.

## Standard Errors

Check the `m:kraft_jsonrpc` module for more information about the JSON-RPC 2.0
error codes and their meaning.

## Examples

```erlang
message({call, add, #{<<"a">> := A, <<"b">> := B}, ID}, State) ->
    Result = A + B,
    {[{result, Result, ID}], State};

message({call, divide, #{<<"a">> := A, <<"b">> := B}, ID}, State) ->
    case B of
        0 -> 
            {[{error, -32602, <<"Division by zero">>, undefined, ID}], State};
        _ -> 
            Result = A / B,
            {[{result, Result, ID}], State}
    end;

message({notification, log_event, #{<<"event">> := Event}, State) ->
    % Handle notification (no response needed)
    io:format("Event: ~p~n", [Event]),
    {[], State};

message({call, unknown_method, _Params, ID}, State) ->
    % Return standard error
    {[{error, method_not_found, ID}], State}.
```

""".
-callback message(kraft_jsonrpc:message(), state()) ->
    {[kraft_jsonrpc:message()], state()}.

-doc """
Optional callback for handling Erlang messages.

This callback is called when the WebSocket process receives an Erlang message.
Useful for handling asynchronous events, timers, or inter-process communication
in your JSON-RPC handler.
""".
-optional_callbacks([info/2]).
-callback info(any(), state()) -> {[kraft_jsonrpc:message()], state()}.

%--- Callbacks -----------------------------------------------------------------

-doc false.
init(Conn, State0) ->
    MState = kraft_ws_util:raw_call(?FUNCTION_NAME, [Conn], State0),
    % FIXME: Ugly to update inner state here?
    {[], State0#{state => MState}}.

-doc false.
handle({text, Data}, State0) ->
    {Replies, State1} = handle_messages(kraft_jsonrpc:decode(Data), State0),
    {[encode(R) || R <- Replies], State1}.

-doc false.
info(Info, State0) ->
    {Replies, State1} = kraft_ws_util:call(info, [Info], State0),
    {[encode(R) || R <- Replies], State1}.

-doc false.
terminate(Reason, _Req, State0) ->
    kraft_ws_util:raw_call(?FUNCTION_NAME, [Reason], State0).

%--- Internal ------------------------------------------------------------------

-doc false.
handle_messages({batch, Messages}, State0) ->
    Unpacked = [unpack(M) || M <- Messages],
    {Replies, State3} = lists:foldl(
        fun(Message, {Rs, State1}) ->
            {R, State2} = handle_message(Message, State1),
            {[Rs, R], State2}
        end,
        {[], State0},
        Unpacked
    ),
    {lists:flatten(Replies), State3};
handle_messages({single, Message}, State0) ->
    handle_message(unpack(Message), State0).

-doc false.
handle_message({internal_error, _, _} = Error, State0) ->
    {[kraft_jsonrpc:format_error(Error)], State0};
handle_message(Message, #{handler := Handler} = State0) ->
    try
        kraft_ws_util:call(message, [Message], State0)
    catch
        error:function_clause:ST ->
            case {Message, ST} of
                {{call, _, _, ID}, [{Handler, message, _, _} | _]} ->
                    ?LOG_ERROR("JSON-RPC unhandled message: ~p", [Message]),
                    {[error_reply(method_not_found, ID)], State0};
                _Else ->
                    {[], State0}
            end
    end.

-doc false.
encode(close = Close) -> Close;
encode({close, _IOData} = Close) -> Close;
encode({close, _Code, _IOData} = Close) -> Close;
encode(Messages) -> {text, kraft_jsonrpc:encode(Messages)}.

-doc false.
error_reply(method_not_found, ID) ->
    kraft_jsonrpc:format_error({internal_error, method_not_found, ID}).

-doc false.
unpack({call, Method, Params, ID}) ->
    {call, kraft_util:attempt_atom(Method), Params, ID};
unpack({notification, Method, Params}) ->
    {notification, kraft_util:attempt_atom(Method), Params};
unpack(Message) ->
    Message.
