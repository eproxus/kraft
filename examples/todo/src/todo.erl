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
-export([get/2]).
-export([post/2]).
-export([delete/2]).

%--- Records -------------------------------------------------------------------

-record(item, {
    id,
    state = unchecked,
    text
}).

%--- Application Callbacks -----------------------------------------------------

start(_StartType, _StartArgs) ->
    init(),
    Ref = kraft:start(#{port => 8095}, [
        {"/assets", kraft_static, #{}},
        {"/", ?MODULE, #{}, #{type => rest}}
    ]),
    {ok, self(), Ref}.

stop(Ref) -> kraft:stop(Ref).

%--- API -----------------------------------------------------------------------

items() -> [item_to_map(Item) || Item <- ets:tab2list(?MODULE)].

new(State, Text) ->
    Atomic = persistent_term:get(?MODULE),
    ID = atomics:add_get(Atomic, 1, 1),
    Item = #item{id = ID, state = State, text = Text},
    ets:insert(?MODULE, Item),
    item_to_map(Item).

state(ID, State) when State == checked; State == unchecked ->
    true = ets:update_element(?MODULE, ID, {#item.state, State}).

text(ID, Text) when is_binary(Text) ->
    true = ets:update_element(?MODULE, ID, {#item.text, Text}).

delete(ID) -> ets:delete(?MODULE, ID).

%--- Kraft Handler -------------------------------------------------------------

get([], Conn0) ->
    {respond, Conn0, {template, "index.html", #{items => items()}}}.

post(
    [<<"items">>],
    #{
        headers := #{<<"hx-request">> := <<"true">>},
        body := {form, [{<<"item">>, Text}]}
    } = Conn0
) ->
    Item = new(unchecked, Text),
    {respond, Conn0, {template, "item.html", Item}};
post([<<"items">>], #{body := {form, [{<<"item">>, Text}]}} = Conn0) ->
    new(unchecked, Text),
    {respond, Conn0, {303, #{<<"location">> => <<"/">>}}};
post(
    [<<"items">>, ID, <<"check">>],
    #{body := {form, [{<<"state">>, <<"on">>}]}} = Conn0
) ->
    state(binary_to_integer(ID), checked),
    {respond, Conn0, 200};
post([<<"items">>, ID, <<"check">>], #{body := undefined} = Conn0) ->
    state(binary_to_integer(ID), unchecked),
    {respond, Conn0, 200};
post(
    [<<"items">>, ID, <<"text">>],
    #{body := {form, [{<<"text">>, Text}]}} = Conn0
) ->
    text(binary_to_integer(ID), Text),
    {respond, Conn0, 200}.

delete([<<"items">>, ID], Conn0) ->
    delete(binary_to_integer(ID)),
    {respond, Conn0, 200}.

%--- Internal ------------------------------------------------------------------

init() ->
    ets:new(?MODULE, [named_table, ordered_set, public, {keypos, #item.id}]),
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

item_to_map(#item{id = ID, state = State, text = Text}) ->
    #{id => ID, state => State, text => Text}.
