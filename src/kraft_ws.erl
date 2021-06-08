-module(kraft_ws).

-behaviour(cowboy_websocket).

% Callbacks
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%--- Types ---------------------------------------------------------------------

-type commands() :: cowboy_websocket:commands().
-type state() :: any().
-type frame() :: ping | pong | {text | binary | ping | pong, binary()}.

-optional_callbacks([handshake/3]).
-callback handshake(kraft:conn(), kraft:params(), state()) ->
    {reply, kraft:status(), kraft:headers(), kraft:body()}
    | {ok, state()}.

-callback init(state()) -> {commands(), state()}.

-callback handle(frame(), state()) -> {commands(), state()}.

-optional_callbacks([info/2]).
-callback info(any(), state()) -> {commands(), state()}.

%--- Callbacks -----------------------------------------------------------------

init(Req, State0) ->
    State1 = kraft_ws_util:callbacks(
        [{handshake, 3}, {info, 2}, {terminate, 2}],
        State0
    ),
    kraft_ws_util:handshake(Req, State1).

websocket_init(State) ->
    call(init, [], State).

websocket_handle(Frame, State) ->
    call(handle, [Frame], State).

websocket_info(Info, State) ->
    call(info, [Info], State).

terminate(_Reason, _Req, #{callbacks := #{{terminate, 2} := false}}) ->
    ok;
terminate(Reason, _Req, #{handler := Handler, state := MState0}) ->
    Handler:terminate(Reason, MState0),
    ok.

%--- API -----------------------------------------------------------------------

call(info, _Args, #{callbacks := #{{info, 2} := false}} = State0) ->
    {[], State0};
call(Func, Args, #{handler := Handler, state := MState0} = State0) ->
    {Commands, MState1} = erlang:apply(Handler, Func, Args ++ [MState0]),
    {Commands, State0#{state => MState1}}.
