-module(todo_index).

-behaviour(kraft_controller).

% API
-export([init/2]).

%--- API -----------------------------------------------------------------------

init(Conn, _State) ->
    Items = [Item#{id => ID} || {ID, Item} <- todo:items()],
    {respond, Conn, {template, "index.html", #{items => Items}}}.
