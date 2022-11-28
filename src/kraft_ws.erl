-module(kraft_ws).

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

-optional_callbacks([handshake/3]).
-callback handshake(kraft:conn(), kraft:params(), state()) ->
    {reply, kraft:status(), kraft:headers(), kraft:body()}
    | {ok, state()}.

-callback init(kraft:conn(), state()) -> {commands(), state()}.

-callback handle(frame(), state()) -> {commands(), state()}.

-optional_callbacks([info/2]).
-callback info(any(), state()) -> {commands(), state()}.

%--- Callbacks -----------------------------------------------------------------

init(Conn, State0) ->
    kraft_ws_util:call(?FUNCTION_NAME, [Conn], State0).

handle(Frame, State0) ->
    kraft_ws_util:call(?FUNCTION_NAME, [Frame], State0).

info(Info, State) ->
    kraft_ws_util:call(?FUNCTION_NAME, [Info], State).

terminate(Reason, _Req, State0) ->
    kraft_ws_util:raw_call(?FUNCTION_NAME, [Reason], State0).
