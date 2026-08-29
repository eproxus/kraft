-module(kraft_ws).

-moduledoc """
Raw WebSocket behaviour for Kraft framework.

This module defines the behaviour for implementing raw WebSocket handlers in Kraft.
It provides a clean interface for handling WebSocket connections with automatic
protocol management, ping/pong support, and state management.

## Callbacks

**Required Callbacks**

- **`init/2`** - Initialize the WebSocket connection
- **`handle/2`** - Handle incoming WebSocket frames
- **`terminate/3`** - Cleanup when connection closes

**Optional Callbacks**

- **`handshake/3`** - Custom handshake logic (authentication, headers, etc.)
- **`info/2`** - Handle Erlang messages sent to the WebSocket process

## Frame Types

WebSocket frames are the different types of messages/data that can be sent and
received through a WebSocket connection. 

The `handle/2` callback receives these frame types:

- **`{text, Data}`** - Text messages from the client (text, JSON, etc.)
- **`{binary, Data}`** - Binary data from the client (images, audio, video, etc.)
- **`ping`** - Ping frame from the client (keep-alive)
- **`pong`** - Pong frame from the client (keep-alive)

## Usage

```erlang
-module(my_websocket_handler).
-behaviour(kraft_ws).

-export([init/2, handle/2, terminate/3]).

init(_Conn, State) ->
    {[{text, <<"Connected to WebSocket!">>}], State}.

handle({text, <<"ping">>}, State) ->
    {[pong], State};
handle({text, Message}, State) ->
    {[{text, [<<"Echo: ">>, Message]}], State};
handle(ping, State) ->
    {[pong], State}.

terminate(_Reason, _Req, _State) ->
    ok.
```

Configure WebSocket routes in your Kraft application:

```erlang
Routes = [
    {"/ws", {ws, my_websocket_handler}, #{}},
    {"/ws/chat", {ws, chat_handler}, #{}, #{
        type => raw,
        ping => #{interval => 30000}
    }}
].

kraft:start(#{port => 8080}, Routes).
```

## State Management

The behaviour automatically manages two types of state:

- **Connection State** - Managed by Kraft (ping timers, connection info)
- **Module State** - Your custom state for business logic

Your callbacks receive and return the module state, while Kraft handles
the connection state internally.

## Error Handling

The behaviour provides automatic error handling:

- **Connection errors** - Automatically handled and logged
- **Frame parsing errors** - Invalid frames are logged and ignored
- **Handler errors** - Exceptions are caught and connection is closed gracefully

## See Also
- `kraft_ws_util` - Core WebSocket utility module
- `kraft_ws_json` - JSON WebSocket behaviour
- `kraft_ws_jsonrpc` - JSON-RPC WebSocket behaviour
- `cowboy_websocket` - Underlying Cowboy WebSocket behaviour
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

%--- Types ---------------------------------------------------------------------

-type commands() :: cowboy_websocket:commands().
-type state() :: any().
-type frame() :: ping | pong | {text | binary | ping | pong, binary()}.

-doc """
Optional callback for custom WebSocket handshake logic.

This callback is called during the WebSocket handshake process and allows you
to implement custom authentication, validation, or response logic.

## Example

```erlang
handshake(Conn, #{token := Token}, State) ->
    case validate_token(Token) of
        {ok, User} -> {ok, State#{user => User}};
        error -> {reply, 401, #{}, <<"Unauthorized">>}
    end.
```
""".
-optional_callbacks([handshake/3]).
-callback handshake(kraft:conn(), kraft:params(), state()) ->
    {reply, kraft:status(), kraft:headers(), kraft:body()}
    | {ok, state()}.

-doc """
Required callback for initializing the WebSocket connection.

This callback is called when a WebSocket connection is established. It's the
perfect place to send initial messages, set up connection state, or perform
any initialization logic.

## Example

```erlang
init(_Conn, State) ->
    % Send welcome message
    Welcome = [{text, <<"Welcome to the chat!">>}],
    
    % Initialize with user count
    UserCount = get_online_users(),
    CountMsg = [{text, [<<"Online users: ">>, integer_to_binary(UserCount)]}],
    
    {Welcome ++ CountMsg, State}.
```
""".
-callback init(kraft:conn(), state()) -> {commands(), state()}.

-doc """
Required callback for handling incoming WebSocket frames.

This callback is called for every WebSocket frame received from the client.
It's where you implement your main WebSocket logic.

## Example

```erlang
handle({text, <<"hello">>}, State) ->
    {[{text, <<"Hello there!">>}], State};

handle({text, <<"echo">>, Data}, State) ->
    {[{text, [<<"Echo: ">>, Data]}], State};

handle(ping, State) ->
    {[pong], State};

handle({binary, ImageData}, State) ->
    % Process binary image data
    ProcessedImage = process_image(ImageData),
    {[{binary, ProcessedImage}], State}.
```

""".
-callback handle(frame(), state()) -> {commands(), state()}.

-doc """
Optional callback for handling Erlang messages.

This callback is called when the WebSocket process receives an Erlang message.
Useful for handling asynchronous events, timers, or inter-process communication.

Example:

```erlang
info({timer, broadcast, Message}, State) ->
    {[{text, Message}], State}.
```

This does not affect the WebSocket connection, it's only used to send messages
to the WebSocket process.
""".
-optional_callbacks([info/2]).
-callback info(any(), state()) -> {commands(), state()}.

%--- Callbacks -----------------------------------------------------------------

-doc false.
init(Conn, State0) ->
    kraft_ws_util:call(?FUNCTION_NAME, [Conn], State0).

-doc false.
handle(Frame, State0) ->
    kraft_ws_util:call(?FUNCTION_NAME, [Frame], State0).

-doc false.
info(Info, State) ->
    kraft_ws_util:call(?FUNCTION_NAME, [Info], State).

-doc false.
terminate(Reason, _Req, State0) ->
    kraft_ws_util:raw_call(?FUNCTION_NAME, [Reason], State0).
