-module(rest_api).

% Callbacks
-export([init/2]).
-export([content_types_provided/2]).
-export([to_json/2]).

%--- Callbacks -----------------------------------------------------------------

init(Req, State) ->
    {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, State) ->
    Body = jsone:encode(#{
        user => <<"User1">>,
        id => 1,
        role => admin,
        access => last_accessed()
    }),
    {Body, Req, State}.

%--- Internal ------------------------------------------------------------------

last_accessed() ->
    SystemTime = erlang:system_time(millisecond),
    Time = calendar:system_time_to_rfc3339(SystemTime, [{unit, millisecond}]),
    list_to_binary(Time).
