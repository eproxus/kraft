-module(kraft_ws_json).

-behaviour(cowboy_websocket).

% Callbacks
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%--- Includes ------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

%--- Types ---------------------------------------------------------------------

-type commands() :: cowboy_websocket:commands().
-type state() :: any().
-type frame() :: {json, jsone:json_term()}.

-callback init(state()) -> {commands(), state()}.
-callback handle(frame(), state()) -> {commands(), state()}.
-callback info(any(), state()) -> {commands(), state()}.
-optional_callbacks([info/2]).

%--- Callbacks -----------------------------------------------------------------

init(Req, #{handler := Handler} = State) ->
    {cowboy_websocket, Req, State#{
        callbacks => kraft_ws_util:callbacks(Handler, [
            {info, 2},
            {terminate, 2}
        ])
    }}.

websocket_init(State) ->
    call(init, [], State).

websocket_handle({text, Data}, State) ->
    try
        call(handle, [{json, decode(Data)}], State)
    catch
        error:badarg:ST ->
            case ST of
                [{jsone_decode,_,_,_}|_] ->
                    ?LOG_WARNING("Bad JSON received: ~p", [cut(Data, 20)]),
                    {[], State};
                _ ->
                    erlang:raise(error, badarg, ST)
            end
    end.

websocket_info(Info, State) ->
    call(info, [Info], State).

terminate(_Reason, _Req, #{callbacks := #{{terminate, 2} := false}}) ->
    ok;
terminate(Reason, _Req, #{handler := Handler, state := MState0}) ->
    Handler:terminate(Reason, MState0),
    ok.

%--- Internal ------------------------------------------------------------------

call(info, _Args, #{callbacks := #{{info, 2} := false}} = State0) ->
    {[], State0};
call(Func, Args, #{handler := Handler, state := MState0} = State0) ->
    {Commands, MState1} = erlang:apply(Handler, Func, Args ++ [MState0]),
    {[encode(C) || C <- Commands], State0#{state => MState1}}.

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
