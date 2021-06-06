-module(kraft_ws_jsonrpc).

-behaviour(cowboy_websocket).

% Callbacks
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

%--- Types ---------------------------------------------------------------------

-type result() :: {result, any(), kraft_jsonrpc:id()}.
-type results() :: [result()].
-type state() :: any().

-callback init(state()) -> state().
-callback message(kraft_jsonrpc:message(), state()) -> {results(), state()}.
-callback info(any(), state()) -> {results(), state()}.
-optional_callbacks([info/2]).

%--- Callbacks -----------------------------------------------------------------

init(Req, #{handler := Handler} = State) ->
    {cowboy_websocket, Req, State#{
        callbacks => kraft_ws_util:callbacks(Handler, [{info, 2}])
    }}.

websocket_init(#{handler := Handler, state := MState0} = State0) ->
    {[], State0#{state => Handler:init(MState0)}}.

websocket_handle({text, JSON}, #{handler := Handler, state := MState0} = State0) ->
    case kraft_jsonrpc:decode(JSON) of
        {single, {internal_error, _, _} = Error} ->
            {[{text, kraft_jsonrpc:encode({single, kraft_jsonrpc:format_error(Error)})}], State0};
        {batch, {internal_error, _, _} = Error} ->
            {[{text, kraft_jsonrpc:encode({single, kraft_jsonrpc:format_error(Error)})}], State0};
        {batch, Messages} ->
            {Replies, State1} = handle_batch([unpack(M) || M <- Messages], State0),
            {[{text, kraft_jsonrpc:encode({batch, Replies})}], State1};
        {single, Message} ->
            {Replies, MState1} = handle_message(Handler, unpack(Message), MState0),
            {[{text, kraft_jsonrpc:encode({single, R})} || R <- Replies], State0#{state => MState1}}
    end.

websocket_info(Info, #{handler := Handler, state := MState0, callbacks := #{{info, 2} := true}} = State0) ->
    {Replies, MState1} = Handler:info(Info, MState0),
    {[{text, kraft_jsonrpc:encode({single, R})} || R <- Replies], State0#{state => MState1}};
websocket_info(_Info, State0) ->
    {[], State0}.

%--- Internal ------------------------------------------------------------------

handle_batch(Messages, #{handler := Handler, state := MState0} = State0) ->
    {Replies, MState3} = lists:foldl(fun
        ({internal_error, _, _} = Error, {Rs, MState1}) ->
            {[Rs, kraft_jsonrpc:format_error(Error)], MState1};
        (Message, {Rs, MState1}) ->
            {R, MState2} = handle_message(Handler, Message, MState1),
            {[Rs, R], MState2}
    end, {[], MState0}, Messages),
    {lists:flatten(Replies), State0#{state => MState3}}.

handle_message(Handler, Message, MState0) ->
    try
        Handler:message(Message, MState0)
    catch
        error:function_clause:ST ->
            case {Message, ST} of
                {{call, _, _, _}, [{Handler, message, _, _}|_]} ->
                    {[kraft_jsonrpc:format_error({internal_error, method_not_found, id(Message)})], MState0};
                _Else ->
                    {[], MState0}
            end
    end.

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

id({call, _Method, _Params, ID}) -> ID;
id({result, _Method, _Params, ID}) -> ID;
id({error, _Code, _Message, _Data, ID}) -> ID;
id(_Message) -> undefined.
