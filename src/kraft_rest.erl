-module(kraft_rest).

-moduledoc """
REST API handler for Kraft framework.

This module provides a simplified REST handler that automatically routes HTTP 
requests to appropriate callback functions based on the HTTP method. 

It implements the `m:kraft_handler` behaviour.

## Callbacks

The following callbacks are available (all optional):

* `get/2` - Handle GET requests
* `post/2` - Handle POST requests  
* `delete/2` - Handle DELETE requests

## Usage 

```erlang
-module(my_rest_handler).
-behaviour(kraft_rest).

-export([get/2]).

 get([<<"users">>], Conn) ->
     Users = fetch_users(),
     {respond, Conn, {json, Users}};

```

## Error Handling ==

* Method not allowed (405) - When a callback function is not implemented
* Not found (404) - When a callback function has no matching clause
* Not implemented (501) - When an unsupported HTTP method is used

## See Also

- `m:kraft_handler` - Base handler module
""".

-behaviour(kraft_handler).

% Callbacks
-export([exec/1]).

-callback get(kraft_conn:path(), kraft_conn:conn()) -> kraft:response().
-callback post(kraft_conn:path(), kraft_conn:conn()) -> kraft:response().
-callback delete(kraft_conn:path(), kraft_conn:conn()) -> kraft:response().
-optional_callbacks([get/2, post/2, delete/2]).

%--- Callbacks -----------------------------------------------------------------

-spec exec(kraft_conn:conn()) -> kraft:response().
exec(Conn0) ->
    #{handler := Module, route := Route} = kraft_conn:'_meta'(Conn0),
    handle(Conn0, Module, Route).

%--- Internal ------------------------------------------------------------------

handle(#{method := Method} = Conn0, Module, Route) ->
    Conn1 = kraft_conn:await_body(Conn0),
    Prefix = kraft_util:split_path(Route),
    Function = method(Method),
    try
        apply(Module, Function, [
            prefix(maps:get(path, Conn1), Prefix), Conn1
        ])
    catch
        error:undef:StackTrace ->
            return_error_code(undef, Module, Function, StackTrace, 405);
        error:function_clause:StackTrace ->
            return_error_code(
                function_clause, Module, Function, StackTrace, 404
            )
    end.

return_error_code(Type, Module, Method, StackTrace, Code) ->
    case StackTrace of
        [{Module, Method, _, _} | _] -> throw(Code);
        _ -> erlang:raise(error, Type, StackTrace)
    end.

method(<<"GET">>) -> get;
method(<<"POST">>) -> post;
method(<<"DELETE">>) -> delete;
method(_) -> throw(501).

prefix([E | L1], [E | L2]) -> prefix(L1, L2);
prefix(L1, [<<"[...]">>]) -> L1.
