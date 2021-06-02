-module(kraft_fallback_h).

-behaviour(cowboy_stream).

% Callbacks
-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

%--- Callbacks -----------------------------------------------------------------

init(StreamID, #{method := Method, path := Path} = Req, Opts) ->
    {Commands0, Next0} = cowboy_stream:init(StreamID, Req, Opts),
    {Commands0, #{next => Next0, method => Method, path => Path}}.

data(StreamID, IsFin, Data, #{next := Next0} = State0) ->
    {Commands0, Next1} = cowboy_stream:data(StreamID, IsFin, Data, Next0),
    {Commands0, State0#{next => Next1}}.

info(StreamID, Info, #{next := Next0} = State0) ->
    {Commands0, Next1} = cowboy_stream:info(StreamID, Info, Next0),
    {insert_fallbacks(Commands0, State0), State0#{next => Next1}}.

terminate(StreamID, Reason, #{next := Next0}) ->
    cowboy_stream:terminate(StreamID, Reason, Next0).

early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

%--- Internal ------------------------------------------------------------------

insert_fallbacks(Commands0, State) ->
    lists:map(fun(C) -> insert_fallback(C, State) end, Commands0).

insert_fallback({response, 404, Headers, <<>>}, State) ->
    Body = kraft_template:render(kraft, "404.html", #{
        req => #{
            method => maps:get(method, State),
            path => maps:get(path, State)
        }
    }),
    ContentLength = integer_to_binary(iolist_size(Body)),
    {response, 404, Headers#{<<"content-length">> => ContentLength}, Body};
insert_fallback(Command, _State) ->
    Command.
