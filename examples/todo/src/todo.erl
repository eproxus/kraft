-module(todo).

-behaviour(application).
-behaviour(kraft_rest).

% Application Callbacks
-export([start/2]).
-export([stop/1]).

% API
-export([items/0]).
-export([new/2]).

% API
-export([get/3]).

%--- Application Callbacks -----------------------------------------------------

start(_StartType, _StartArgs) ->
    init(),
    Ref = kraft:start(#{port => 8095}, [
        {"/[...]", ?MODULE, #{}, #{type => rest}},
        {"/", kraft_static, #{}}
    ]),
    {ok, self(), Ref}.

stop(Ref) -> kraft:stop(Ref).

%--- API -----------------------------------------------------------------------

items() -> ets:tab2list(?MODULE).

new(State, Text) ->
    Atomic = persistent_term:get(?MODULE),
    ID = atomics:add_get(Atomic, 1, 1),
    ets:insert(?MODULE, {ID, #{state => State, text => Text}}).

%--- Kraft Handler -------------------------------------------------------------

get(Conn0, [], _Extra) ->
    Items = [Item#{id => ID} || {ID, Item} <- todo:items()],
    {respond, Conn0, {template, "index.html", #{items => Items}}}.

%--- Internal ------------------------------------------------------------------

init() ->
    ets:new(?MODULE, [named_table, ordered_set]),
    persistent_term:put(?MODULE, atomics:new(1, [])),
    [
        new(State, Text)
     || {State, Text} <- [
            {checked, <<"Implement TODO app">>},
            {unchecked, <<"Buy milk">>},
            {unchecked, <<"Call the president">>}
        ]
    ],
    ok.
