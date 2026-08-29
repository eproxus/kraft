-module(kraft_ws_util).

-moduledoc """
WebSocket utility module for Kraft framework.

This module provides a bridge between Kraft's WebSocket handling and Cowboy's
WebSocket implementation. Doing the following:

* **WebSocket protocol handling** - Manages WebSocket handshake and lifecycle
* **Callback management** - Automatically detects and manages optional callbacks
* **Ping/Pong support** - Configurable heartbeat mechanism for connection health
* **State management** - Handles connection state and module state separately
* **Multiple protocol types** - Supports raw, JSON, and JSON-RPC WebSocket types

## Protocol Types
- **m:kraft_ws** - Raw WebSocket frames (default)
- **m:kraft_ws_json** - JSON-encoded WebSocket messages
- **m:kraft_ws_jsonrpc** - JSON-RPC protocol over WebSocket

If this doesnt work for, you can implement the `cowboy_websocket` behaviour.

## How It Works

The module acts as a middleware that:
1. Sets up WebSocket connections with configurable options
2. Manages the handshake process with optional custom handshake logic
3. Routes incoming WebSocket frames to appropriate handlers
4. Maintains ping/pong timers for connection health
5. Provides a clean interface between Kraft and Cowboy


## See Also
- `m:kraft_ws` - Raw WebSocket handling
- `m:kraft_ws_json` - JSON WebSocket handling  
- `m:kraft_ws_jsonrpc` - JSON-RPC WebSocket handling
- `cowboy_websocket` - Cowboy WebSocket behaviour
""".

% API
-export([setup/4]).
-export([callbacks/2]).
-export([handshake/2]).
-export([call/3]).
-export([raw_call/3]).

% Callbacks
-behaviour(cowboy_websocket).
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%--- Types ---------------------------------------------------------------------

-type ws_type() :: raw | json | json_rpc.
-type ws_opts() :: #{
    type => ws_type(),
    ping => #{interval => non_neg_integer()} | disabled
}.
-type ws_state() :: #{
    opts => ws_opts(),
    app => atom(),
    module => module(),
    handler => module(),
    state => term(),
    callbacks => #{atom() => boolean()},
    conn => kraft_conn:conn(),
    ping => #{timer => reference() | undefined, target => non_neg_integer()}
}.

%--- API -----------------------------------------------------------------------

-doc """
Setup WebSocket handler with configuration options.
""".
-spec setup(ws_opts(), atom(), module(), term()) -> {module(), map()}.
setup(UserOpts, App, Handler, MState) ->
    Opts = maps:merge(default_opts(), UserOpts),
    {
        ?MODULE,
        #{
            opts => Opts,
            app => App,
            module => callback_module(Opts),
            handler => Handler,
            state => MState
        }
    }.

-doc """
Check which callbacks are implemented by the handler module.

This function examines the handler module and determines which optional
callbacks are actually implemented, storing this information in the state.
""".
-spec callbacks([{atom(), non_neg_integer()}], ws_state()) -> ws_state().
callbacks(Callbacks, #{handler := Handler} = State0) ->
    CheckCallback = fun(C, Cs) ->
        maps:put(C, lists:member(C, Handler:module_info(exports)), Cs)
    end,
    Exported = lists:foldl(CheckCallback, #{}, Callbacks),
    State0#{callbacks => Exported}.

-doc """
Handle WebSocket handshake with optional custom logic.

If the handler implements a custom handshake callback, it calls that; otherwise,
it proceeds directly to WebSocket mode.
""".
-spec handshake(cowboy_req:req(), ws_state()) ->
    {cowboy_websocket, cowboy_req:req(), ws_state()}
    | {ok, cowboy_req:req(), ws_state()}.
handshake(Req, #{callbacks := #{{handshake, 3} := false}} = State0) ->
    {cowboy_websocket, Req, State0};
handshake(Req, #{handler := Handler, state := MState0} = State0) ->
    Conn0 = kraft_conn:new(Req, State0),
    case Handler:handshake({Req, MState0}, kraft_conn:params(Conn0), MState0) of
        {reply, Code, Headers, Body} ->
            Resp = cowboy_req:reply(Code, Headers, Body, Req),
            {ok, Resp, State0};
        {ok, MState1} ->
            {cowboy_websocket, Req, State0#{state => MState1}};
        {ok, Headers, MState1} ->
            Req1 = cowboy_req:set_resp_headers(Headers, Req),
            {cowboy_websocket, Req1, State0#{state => MState1}}
    end.

-doc """
Call handler function with automatic callback checking.

This function calls the specified handler function if it's implemented,
otherwise returns empty commands. It automatically manages the module state.

## Supported Functions

- `info/2` - Handle Erlang messages
- `terminate/2` - Handle termination
- Any other function implemented by the handler
""".
-spec call(atom(), [term()], ws_state()) -> {list(), ws_state()}.
call(info, _Args, #{callbacks := #{{info, 2} := false}} = State0) ->
    {[], State0};
call(terminate, _Args, #{callbacks := #{{terminate, 2} := false}} = State0) ->
    {[], State0};
call(Func, Args, State0) ->
    {Commands, MState1} = raw_call(Func, Args, State0),
    {Commands, State0#{state => MState1}}.

-doc """
Call handler function directly without state management.
""".
-spec raw_call(atom(), [term()], ws_state()) -> term().
raw_call(terminate, _Args, #{callbacks := #{{terminate, 2} := false}}) ->
    ok;
raw_call(Func, Args, #{handler := Handler, state := MState0}) ->
    erlang:apply(Handler, Func, Args ++ [MState0]).

%--- Callbacks -----------------------------------------------------------------

-doc false.
init(Req, State0) ->
    State1 = kraft_ws_util:callbacks(
        [{handshake, 3}, {info, 2}, {terminate, 2}],
        State0
    ),
    Conn = kraft_conn:new(Req, State0),
    kraft_ws_util:handshake(Req, State1#{conn => Conn}).

-doc false.
websocket_init(#{conn := Conn} = State0) ->
    State1 = trigger_ping(State0),
    module(init, [Conn], State1).

-doc false.
websocket_handle(pong, #{ping := #{}} = State0) ->
    {[], State0};
websocket_handle(Frame, State0) ->
    module(handle, [Frame], State0).

-doc false.
websocket_info('$kraft_ws_ping', State0) ->
    State1 = trigger_ping(State0),
    {[ping], State1};
websocket_info(Info, State0) ->
    module(info, [Info], State0).

-doc false.
terminate(Reason, Req, State0) ->
    cancel_ping(State0),
    module(terminate, [Reason, Req], State0),
    ok.

%--- Internal ------------------------------------------------------------------

-doc false.
default_opts() -> #{type => raw, ping => #{interval => 30_000}}.

-doc false.
callback_module(#{type := raw}) -> kraft_ws;
callback_module(#{type := json}) -> kraft_ws_json;
callback_module(#{type := json_rpc}) -> kraft_ws_jsonrpc;
callback_module(#{type := Other}) -> error({invalid_kraft_ws_type, Other}).

-doc false.
trigger_ping(#{opts := #{ping := disabled}} = State0) ->
    State0;
trigger_ping(#{ping := #{target := Last}} = State0) ->
    #{opts := #{ping := #{interval := Interval}}} = State0,
    Target = Last + Interval,
    Ref = erlang:send_after(Target, self(), '$kraft_ws_ping', [{abs, true}]),
    mapz:deep_merge(State0, #{ping => #{timer => Ref, target => Target}});
trigger_ping(State0) ->
    trigger_ping(State0#{
        ping => #{target => erlang:monotonic_time(millisecond)}
    }).

-doc false.
cancel_ping(#{ping := #{timer := Ref} = Ping} = State0) ->
    erlang:cancel_timer(Ref, [{info, false}]),
    State0#{ping => Ping#{timer => undefined}};
cancel_ping(State0) ->
    State0.

-doc false.
module(Func, Args, #{module := Module} = State0) ->
    erlang:apply(Module, Func, Args ++ [State0]).
