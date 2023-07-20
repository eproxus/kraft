-module(todo_index).

-behaviour(kraft_controller).

% API
-export([init/2]).

%--- API -----------------------------------------------------------------------

init(Conn, _State) ->
    {respond, Conn, {template, "index.html", #{items => todo:items()}}}.
