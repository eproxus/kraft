-module(kraft_ws_json).

% Callbacks
-export([init/1]).
-ignore_xref({init, 1}).
-export([handle/2]).
-ignore_xref({handle, 2}).
-export([info/2]).
-ignore_xref({info, 2}).
-export([terminate/3]).
-ignore_xref({terminate, 3}).

%--- Includes ------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

%--- Types ---------------------------------------------------------------------

-type commands() :: cowboy_websocket:commands().
-type state() :: any().
-type frame() :: {json, jsone:json_term()}.

-optional_callbacks([handshake/3]).
-callback handshake(kraft:conn(), kraft:params(), state()) ->
    {reply, kraft:status(), kraft:headers(), kraft:body()}
    | {ok, state()}.

-callback init(state()) -> {commands(), state()}.

-callback handle(frame(), state()) -> {commands(), state()}.

-optional_callbacks([info/2]).
-callback info(any(), state()) -> {commands(), state()}.

%--- Callbacks -----------------------------------------------------------------

init(State0) ->
    call(?FUNCTION_NAME, [], State0).

handle({text, Data}, State0) ->
    try
        call(?FUNCTION_NAME, [{json, decode(Data)}], State0)
    catch
        error:badarg:ST ->
            case ST of
                [{jsone_decode, _, _, _} | _] ->
                    ?LOG_WARNING("Bad JSON received: ~p", [cut(Data, 20)]),
                    {[], State0};
                _ ->
                    erlang:raise(error, badarg, ST)
            end
    end.

info(Info, State) ->
    call(?FUNCTION_NAME, [Info], State).

terminate(Reason, _Req, State0) ->
    kraft_ws_util:raw_call(?FUNCTION_NAME, [Reason], State0).

%--- Internal ------------------------------------------------------------------

call(Func, Args, State0) ->
    {Commands, State1} = kraft_ws_util:call(Func, Args, State0),
    {[encode(C) || C <- Commands], State1}.

encode({json, JSON}) -> {text, jsone:encode(JSON)};
encode({text, {kraft_template, _Headers, Body}}) -> {text, Body};
encode({text, Text}) -> {text, Text};
encode({binary, Binary}) -> {binary, Binary}.

decode(Binary) ->
    jsone:decode(Binary, [{keys, attempt_atom}]).

cut(Data, Len) ->
    case Data of
        Data when byte_size(Data) =< Len -> Data;
        <<Prefix:Len/binary, _/binary>> -> <<Prefix/binary, "...">>
    end.
