-module(kraft_ws_jsonrpc).

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

-optional_callbacks([handshake/3]).
-callback handshake(kraft:conn(), kraft:params(), state()) ->
    {reply, kraft:status(), kraft:headers(), kraft:body()}
    | {ok, state()}.

-callback init(kraft:conn(), state()) -> state().

-callback message(kraft_jsonrpc:message(), state()) ->
    {[kraft_jsonrpc:message()], state()}.

-optional_callbacks([info/2]).
-callback info(any(), state()) -> {[kraft_jsonrpc:message()], state()}.

%--- Callbacks -----------------------------------------------------------------

init(Conn, State0) ->
    MState = kraft_ws_util:raw_call(?FUNCTION_NAME, [Conn], State0),
    % FIXME: Ugly to update inner state here?
    {[], State0#{state => MState}}.

handle({text, Data}, State0) ->
    {Replies, State1} = handle_messages(kraft_jsonrpc:decode(Data), State0),
    {[encode(R) || R <- Replies], State1}.

info(Info, State0) ->
    {Replies, State1} = kraft_ws_util:call(info, [Info], State0),
    {[encode(R) || R <- Replies], State1}.

terminate(Reason, _Req, State0) ->
    kraft_ws_util:raw_call(?FUNCTION_NAME, [Reason], State0).

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
    handle_message(unpack(Message), State0).

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

encode(close = Close) -> Close;
encode({close, _IOData} = Close) -> Close;
encode({close, _Code, _IOData} = Close) -> Close;
encode(Messages) -> {text, kraft_jsonrpc:encode(Messages)}.

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
