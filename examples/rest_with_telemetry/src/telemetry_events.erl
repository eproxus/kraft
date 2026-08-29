-module(telemetry_events).

-export([setup/0, teardown/0]).
-export([handle_request/4, get_events/0, clear_events/0]).

-define(TRACKED_ENDPOINTS, [
    {<<"/api/v1/example/endpoint">>, <<"GET">>},
    {<<"/api/v1/error">>, <<"GET">>}
]).

-define(EVENTS_TABLE, telemetry_events_table).

setup() ->
    ets:new(?EVENTS_TABLE, [named_table, public, ordered_set]),

    telemetry:attach_many(
        <<"cowboy-requests">>,
        [
            [cowboy, request, start],
            [cowboy, request, stop],
            [cowboy, request, exception],
            [cowboy, request, early_error]
        ],
        fun ?MODULE:handle_request/4,
        #{}
    ).

teardown() ->
    telemetry:detach(<<"cowboy-requests">>),
    catch ets:delete(?EVENTS_TABLE).

%--- Request Event Handlers ---------------------------------------------------

handle_request([cowboy, request, Event], Meas, Meta, _Cfg) ->
    case should_track_request(Meta) of
        true ->
            store_event(Event, Meta, Meas);
        false ->
            ok
    end,
    ok;
handle_request(_Event, _Meas, _Meta, _Cfg) ->
    ok.

%--- Helper Functions ---------------------------------------------------------

should_track_request(Meta) ->
    case maps:get(req, Meta, #{}) of
        #{path := RequestedPath, method := RequestedMethod} ->
            lists:any(
                fun({Path, Method}) ->
                    RequestedMethod =:= Method andalso
                        RequestedPath =:= Path
                end,
                ?TRACKED_ENDPOINTS
            );
        _ ->
            false
    end.

%% Store telemetry event in ETS table
store_event(EventType, Meta, Measurements) ->
    ConnPid = maps:get(pid, Meta, self()),
    StreamId = maps:get(streamid, Meta, undefined),
    RequestId = {ConnPid, StreamId},
    Req = maps:get(req, Meta, #{}),
    Path = maps:get(path, Req, <<"unknown">>),
    Method = maps:get(method, Req, <<"unknown">>),

    % Convert complex types to JSON-friendly formats
    IdString = io_lib:format("~p", [{RequestId, EventType}]),
    RequestIdString = io_lib:format("~p", [RequestId]),
    ConnPidString = io_lib:format("~p", [ConnPid]),
    JsonMeta = list_to_binary(io_lib:format("~p", [Meta])),

    Event = #{
        id => list_to_binary(IdString),
        request_id => list_to_binary(RequestIdString),
        streamid => StreamId,
        connection_pid => list_to_binary(ConnPidString),
        event_type => EventType,
        path => Path,
        method => Method,
        meta => JsonMeta,
        measurements => Measurements
    },

    #{id := Id} = Event,
    ets:insert(?EVENTS_TABLE, {Id, Event}).

get_events() ->
    try
        case ets:tab2list(?EVENTS_TABLE) of
            [] ->
                [];
            Events ->
                SortedEvents = lists:reverse(lists:keysort(1, Events)),
                [Event || {_Id, Event} <- SortedEvents]
        end
    catch
        error:badarg ->
            []
    end.

clear_events() ->
    ets:delete_all_objects(?EVENTS_TABLE).
