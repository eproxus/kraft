-module(rest).

-behaviour(application).
-behaviour(kraft_rest).

% Application Callbacks
-export([start/2]).
-export([stop/1]).

% REST API
-export([get/2]).

%--- Application Callbacks -----------------------------------------------------

start(_StartType, _StartArgs) ->
    Ref = kraft:start(#{port => 8094}, [
        {"/api/v1", ?MODULE, #{}, #{type => rest}},
        {"/", kraft_static, #{}}
    ]),
    {ok, self(), Ref}.

stop(Ref) -> kraft:stop(Ref).

%--- REST API ------------------------------------------------------------------

get([<<"example">>, <<"endpoint">>], Conn0) ->
    {respond, Conn0,
        {json, #{
            user => <<"User1">>,
            id => 1,
            role => admin,
            access => last_accessed()
        }}}.

%--- Internal ------------------------------------------------------------------

last_accessed() ->
    SystemTime = erlang:system_time(millisecond),
    Time = calendar:system_time_to_rfc3339(SystemTime, [{unit, millisecond}]),
    list_to_binary(Time).
