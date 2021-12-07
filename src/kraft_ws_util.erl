-module(kraft_ws_util).

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

%--- API -----------------------------------------------------------------------

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

callbacks(Callbacks, #{handler := Handler} = State0) ->
    CheckCallback = fun(C, Cs) ->
        maps:put(C, lists:member(C, Handler:module_info(exports)), Cs)
    end,
    Exported = lists:foldl(CheckCallback, #{}, Callbacks),
    State0#{callbacks => Exported}.

handshake(Req, #{callbacks := #{{handshake, 3} := false}} = State0) ->
    {cowboy_websocket, Req, State0};
handshake(Req, #{handler := Handler, state := MState0} = State0) ->
    Conn = kraft_conn:new(Req, State0),
    case Handler:handshake({Req, MState0}, kraft_conn:params(Conn), MState0) of
        {reply, Code, Headers, Body} ->
            Resp = cowboy_req:reply(Code, Headers, Body, Req),
            {ok, Resp, State0};
        {ok, MState1} ->
            {cowboy_websocket, Req, State0#{state => MState1}}
    end.

call(info, _Args, #{callbacks := #{{info, 2} := false}} = State0) ->
    {[], State0};
call(terminate, _Args, #{callbacks := #{{terminate, 2} := false}} = State0) ->
    {[], State0};
call(Func, Args, State0) ->
    {Commands, MState1} = raw_call(Func, Args, State0),
    {Commands, State0#{state => MState1}}.

raw_call(terminate, _Args, #{callbacks := #{{terminate, 2} := false}}) ->
    ok;
raw_call(Func, Args, #{handler := Handler, state := MState0}) ->
    erlang:apply(Handler, Func, Args ++ [MState0]).

%--- Callbacks -----------------------------------------------------------------

init(Req, State0) ->
    State1 = kraft_ws_util:callbacks(
        [{handshake, 3}, {info, 2}, {terminate, 2}],
        State0
    ),
    Conn = kraft_conn:new(Req, State0),
    kraft_ws_util:handshake(Req, State1#{conn => Conn}).

websocket_init(#{conn := Conn} = State0) ->
    State1 = trigger_ping(State0),
    module(init, [Conn], State1).

websocket_handle(pong, #{ping := #{}} = State0) ->
    {[], State0};
websocket_handle(Frame, State0) ->
    module(handle, [Frame], State0).

websocket_info('$kraft_ws_ping', State0) ->
    State1 = trigger_ping(State0),
    {[ping], State1};
websocket_info(Info, State0) ->
    module(info, [Info], State0).

terminate(Reason, Req, State0) ->
    cancel_ping(State0),
    module(terminate, [Reason, Req], State0),
    ok.

%--- Internal ------------------------------------------------------------------

default_opts() -> #{type => raw, ping => #{interval => 30_000}}.

callback_module(#{type := raw}) -> kraft_ws;
callback_module(#{type := json}) -> kraft_ws_json;
callback_module(#{type := json_rpc}) -> kraft_ws_jsonrpc;
callback_module(#{type := Other}) -> error({invalid_kraft_ws_type, Other}).

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

cancel_ping(#{ping := #{timer := Ref} = Ping} = State0) ->
    erlang:cancel_timer(Ref, [{info, false}]),
    State0#{ping => Ping#{timer => undefined}};
cancel_ping(State0) ->
    State0.

module(Func, Args, #{module := Module} = State0) ->
    erlang:apply(Module, Func, Args ++ [State0]).
