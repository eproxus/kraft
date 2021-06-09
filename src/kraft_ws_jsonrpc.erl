-module(kraft_ws_jsonrpc).

-behaviour(cowboy_websocket).

% Callbacks
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%--- Types ---------------------------------------------------------------------

-type state() :: any().

-optional_callbacks([handshake/3]).
-callback handshake(kraft:conn(), kraft:params(), state()) ->
    {reply, kraft:status(), kraft:headers(), kraft:body()}
    | {ok, state()}.

-callback init(state()) ->
    state().

-callback message(kraft_jsonrpc:message(), state()) ->
    {[kraft_jsonrpc:message()], state()}.

-optional_callbacks([info/2]).
-callback info(any(), state()) ->
    {[kraft_jsonrpc:message()], state()}.

%--- Callbacks -----------------------------------------------------------------

init(Req, State0) ->
    State1 = kraft_ws_util:callbacks(
        [{handshake, 3}, {info, 2}, {terminate, 2}],
        State0
    ),
    kraft_ws_util:handshake(Req, State1).

websocket_init(#{handler := Handler, state := MState0} = State0) ->
    {[], State0#{state => Handler:init(MState0)}}.

websocket_handle({text, JSON}, State0) ->
    {Replies, State1} = handle_messages(kraft_jsonrpc:decode(JSON), State0),
    {[encode(R) || R <- Replies], State1}.

websocket_info(Info, #{callbacks := #{{info, 2} := true}} = State0) ->
    {Replies, State1} = call(info, [Info], State0),
    {[encode(R) || R <- Replies], State1};
websocket_info(_Info, State0) ->
    {[], State0}.

terminate(_Reason, _Req, #{callbacks := #{{terminate, 2} := false}}) ->
    ok;
terminate(Reason, _Req, #{handler := Handler, state := MState0}) ->
    Handler:terminate(Reason, MState0),
    ok.

%--- Internal ------------------------------------------------------------------

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
    {[lists:flatten(Replies)], State3};
handle_messages({single, Message}, State0) ->
    handle_message(Message, State0).

handle_message({internal_error, _, _} = Error, State0) ->
    {[kraft_jsonrpc:format_error(Error)], State0};
handle_message(Message, #{handler := Handler} = State0) ->
    try
        call(message, [Message], State0)
    catch
        error:function_clause:ST ->
            case {Message, ST} of
                {{call, _, _, ID}, [{Handler, message, _, _} | _]} ->
                    {[error_reply(method_not_found, ID)], State0};
                _Else ->
                    {[], State0}
            end
    end.

call(Func, Args, #{handler := Handler, state := MState0} = State0) ->
    {Replies, MState1} = erlang:apply(Handler, Func, Args ++ [MState0]),
    {Replies, State0#{state => MState1}}.

encode(Messages) ->
    {text, kraft_jsonrpc:encode(Messages)}.

error_reply(method_not_found, ID) ->
    kraft_jsonrpc:format_error({internal_error, method_not_found, ID}).

unpack({call, Method, Params, ID}) ->
    {call, attempt_atom(Method), Params, ID};
unpack({notification, Method, Params}) ->
    {notification, attempt_atom(Method), Params};
unpack(Message) ->
    Message.

attempt_atom(Binary) when is_binary(Binary) ->
    try
        binary_to_existing_atom(Binary)
    catch
        error:badarg -> Binary
    end.
