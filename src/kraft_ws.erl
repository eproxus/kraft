-module(kraft_ws).

-behaviour(cowboy_websocket).

% Callbacks
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

%--- Types ---------------------------------------------------------------------

-type commands() :: cowboy_websocket:commands().
-type state() :: any().
-type frame() :: ping | pong | {text | binary | ping | pong, binary()}.

-callback init(state()) -> {commands(), state()}.
-callback handle(frame(), state()) -> {commands(), state()}.
-callback info(any(), state()) -> {commands(), state()}.
-optional_callbacks([info/2]).

%--- Callbacks -----------------------------------------------------------------

init(Req, #{handler := Handler} = State) ->
    {cowboy_websocket, Req, State#{
        callbacks => kraft_ws_util:callbacks(Handler, [{info, 2}])
    }}.

websocket_init(State) ->
    call(init, [], State).

websocket_handle(Frame, State) ->
    call(handle, [Frame], State).

websocket_info(Info, State) ->
    call(info, [Info], State).

%--- API -----------------------------------------------------------------------

call(info, _Args, #{callbacks := #{{info, 2} := false}} = State0) ->
    {[], State0};
call(Func, Args, #{handler := Handler, state := MState0} = State0) ->
    {Commands, MState1} = erlang:apply(Handler, Func, Args ++ [MState0]),
    {Commands, State0#{state => MState1}}.
