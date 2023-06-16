-module(kraft_controller).

-behaviour(kraft_handler).

% API
-export([exec/1]).

%--- Types ---------------------------------------------------------------------

-callback init(kraft:conn(), State :: any()) -> kraft:response().

%--- API -----------------------------------------------------------------------

exec(Conn0) ->
    #{handler := Module, state := State} = kraft_conn:'_meta'(Conn0),
    Module:init(Conn0, State).
