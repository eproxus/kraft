-module(rest_with_telemetry).

-behaviour(application).
-behaviour(kraft_rest).

% Application Callbacks
-export([start/2]).
-export([stop/1]).

% REST API
-export([get/2]).
-export([delete/2]).
%--- Application Callbacks -----------------------------------------------------

start(_StartType, _StartArgs) ->
    telemetry_events:setup(),
    Ref = kraft:start(
        #{
            port => 8096,
            stream_handlers => [cowboy_telemetry_h, cowboy_stream_h]
        },
        [
            {"/api/v1", ?MODULE, #{}, #{type => rest}},
            {"/", kraft_static, #{}}
        ]
    ),
    {ok, self(), Ref}.

stop(Ref) ->
    telemetry_events:teardown(),
    kraft:stop(Ref).

%--- REST API ------------------------------------------------------------------

get([<<"example">>, <<"endpoint">>], Conn0) ->
    {respond, Conn0,
        {json, #{
            user => <<"User1">>,
            id => 1,
            role => admin,
            access => last_accessed()
        }}};
get([<<"telemetry">>, <<"events">>], Conn0) ->
    Events = telemetry_events:get_events(),
    {respond, Conn0,
        {json, #{
            events => Events,
            count => length(Events),
            timestamp => last_accessed()
        }}};
get([<<"error">>], Conn0) ->
    {invalid_response_format, Conn0, this_will_crash_cowboy};
get(_, Conn0) ->
    {respond, Conn0, {status, 404}}.

delete([<<"telemetry">>, <<"events">>], Conn0) ->
    telemetry_events:clear_events(),
    {respond, Conn0, {json, <<"ok">>}};
delete(_, Conn0) ->
    {respond, Conn0, {status, 404}}.
%--- Internal ------------------------------------------------------------------
last_accessed() ->
    SystemTime = erlang:system_time(millisecond),
    Time = calendar:system_time_to_rfc3339(SystemTime, [{unit, millisecond}]),
    list_to_binary(Time).
