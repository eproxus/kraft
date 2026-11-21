-module(kraft_ws_json).

-moduledoc """
JSON WebSocket behaviour for Kraft framework.

It automatically handles JSON encoding/decoding, making it perfect
for APIs, chat applications, and any WebSocket communication that uses JSON.

## How It Works

The `kraft_ws_json` behaviour extends `kraft_ws` with JSON-specific functionality:

1. **Receives text frames** from WebSocket clients
2. **Automatically decodes JSON** to Erlang terms
3. **Calls your handler** with decoded data
4. **Encodes your responses** back to JSON
5. **Sends JSON text frames** to the client

```
Client JSON → WebSocket Text Frame → kraft_ws_json → Decode → Your Handler
Your Handler → Response → kraft_ws_json → Encode → WebSocket Text Frame → Client
```

## Callbacks

### Required Callbacks

- **`init/2`** - Initialize the JSON WebSocket connection
- **`handle/2`** - Handle incoming JSON messages
- **`terminate/3`** - Cleanup when connection closes

### Optional Callbacks

- **`handshake/3`** - Custom handshake logic (authentication, validation)
- **`info/2`** - Handle Erlang messages sent to the WebSocket process

## Usage

```erlang
-module(chat_json_handler).
-behaviour(kraft_ws_json).

-export([init/2, handle/2, terminate/3]).

init(_Conn, State) ->
    Welcome = #{type => <<"system">>, message => <<"Connected to chat!">>},
    {[{json, Welcome}], State}.

handle({json, #{<<"type">> := <<"message">>, <<"text">> := Text}}, State) ->
    % Echo the message back
    Response = #{type => <<"echo">>, text => Text, timestamp => erlang:system_time()},
    {[{json, Response}], State};

handle(ping, State) ->
    % WebSocket ping
    {[pong], State}.

terminate(_Reason, _Req, _State) ->
    ok.
```

Configure JSON WebSocket routes in your Kraft application:

```erlang
Routes = [
    {"/ws/chat", {ws, chat_json_handler}, #{}, #{
        type => json,
        ping => #{interval => 30000}
    }}
].

kraft:start(#{port => 8080}, Routes).
```

## Error Handling

The behaviour automatically handles JSON errors:

- **Invalid JSON** - Logged as warning, frame is ignored
- **Decoding errors** - Gracefully handled, connection continues
- **Encoding errors** - Logged and handled appropriately

## See Also
- `m:kraft_ws` - Base WebSocket behaviour
- `m:kraft_ws_util` - Core WebSocket utility module
- `m:kraft_ws_jsonrpc` - JSON-RPC WebSocket behaviour
- `m:kraft_json` - JSON encoding/decoding utilities
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

%--- Includes ------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

%--- Types ---------------------------------------------------------------------

-type commands() :: [frame()].
-type state() :: any().
-type frame() ::
    kraft_json:body_json()
    | cow_ws:frame()
    | {active, boolean()}
    | {deflate, boolean()}
    | {set_options, map()}
    | {shutdown_reason, any()}.

-doc """
Optional callback for custom WebSocket handshake logic.

Check the `c:kraft_ws:handshake/3` callback for more information.
""".
-optional_callbacks([handshake/3]).
-callback handshake(kraft:conn(), kraft_conn:params(), state()) ->
    {reply, kraft:status(), kraft:headers(), kraft:body()}
    | {ok, state()}.

-doc """
Required callback for initializing the JSON WebSocket connection.

This callback is called when a WebSocket connection is established. It's the
perfect place to send initial JSON messages, set up connection state, or
perform any initialization logic.

Check the `c:kraft_ws:init/2` callback for more information.
""".
-callback init(kraft:conn(), state()) -> {commands(), state()}.

-doc """
Required callback for handling incoming JSON WebSocket messages.

This callback is called for every JSON message received from the client.
The JSON is automatically decoded to Erlang terms, so you work with native
Erlang data structures.

## Frame Types
- **`{json, Data}`** - Decoded JSON data as Erlang terms
  - `Data` is the parsed JSON (maps, lists, atoms, etc.)
  - Use for: API calls, structured data, commands

### Control Frames
Check the `m:kraft_ws` module for more information about the WebSocket control 
frames.

## Example

```erlang
handle({json, #{<<"type">> := <<"message">>, <<"text">> := Text}}, State) ->
    % Handle chat message
    Response = #{
        type => <<"echo">>,
        text => Text,
        timestamp => erlang:system_time()
    },
    {[{json, Response}], State};

handle({json, #{<<"type">> := <<"ping">>}}, State) ->
    % Handle JSON ping
    Pong = #{type => <<"pong">>, timestamp => erlang:system_time()},
    {[{json, Pong}], State};

handle(ping, State) ->
    % Handle WebSocket ping
    {[pong], State};

handle({binary, ImageData}, State) ->
    % Handle binary data (bypasses JSON)
    {[{binary, process_image(ImageData)}], State}.
```

""".
-callback handle(frame(), state()) -> {commands(), state()}.

-doc """
Optional callback for handling Erlang messages.

This callback is called when the WebSocket process receives an Erlang message.
Useful for handling asynchronous events, timers, or inter-process communication.

Check the `c:kraft_ws:info/2` callback for more information.
""".
-optional_callbacks([info/2]).
-callback info(any(), state()) -> {commands(), state()}.

%--- Callbacks -----------------------------------------------------------------

-doc false.
init(Conn, State0) -> call(?FUNCTION_NAME, [Conn], State0).

-doc false.
handle({text, Data}, State0) ->
    try
        call(?FUNCTION_NAME, [{json, kraft_json:decode(Data)}], State0)
    catch
        error:badarg ->
            ?LOG_WARNING("Bad JSON received: ~p", [cut(Data, 200)]),
            {[], State0}
    end.

-doc false.
info(Info, State) -> call(?FUNCTION_NAME, [Info], State).

-doc false.
terminate(Reason, _Req, State0) ->
    kraft_ws_util:raw_call(?FUNCTION_NAME, [Reason], State0).

%--- Internal ------------------------------------------------------------------

-doc false.
call(Func, Args, State0) ->
    {Commands, State1} = kraft_ws_util:call(Func, Args, State0),
    {[encode(C) || C <- Commands], State1}.

-doc false.
encode(close = Close) -> Close;
encode({close, _IOData} = Close) -> Close;
encode({close, _Code, _IOData} = Close) -> Close;
encode({json, JSON}) -> {text, kraft_json:encode(JSON)};
encode({text, Text}) -> {text, Text};
encode({binary, Binary}) -> {binary, Binary}.

-doc false.
cut(Data, Len) ->
    case Data of
        Data when byte_size(Data) =< Len -> Data;
        <<Prefix:Len/binary, _/binary>> -> <<Prefix/binary, "...">>
    end.
