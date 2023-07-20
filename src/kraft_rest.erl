-module(kraft_rest).

-behaviour(kraft_handler).

% Callbacks
-export([exec/1]).

-type path() :: list(binary()).
-type extra() :: #{}.

-callback get(kraft_conn:conn(), path(), extra()) -> kraft:response().
-callback post(kraft_conn:conn(), path(), extra()) -> kraft:response().
-optional_callbacks([get/3, post/3]).

%--- Callbacks -----------------------------------------------------------------

exec(Conn0) ->
    #{handler := Module, route := Route} = kraft_conn:'_meta'(Conn0),
    handle(Conn0, Module, Route).

%--- Internal ------------------------------------------------------------------

handle(Conn0, Module, Route) ->
    {RawMethod, Conn1} = kraft_conn:method(Conn0),
    Method = method(RawMethod),
    try
        % FIXME: Deal with body
        {_Body, Conn2} = kraft_conn:body(Conn1),
        {Path, Conn3} = kraft_conn:path(Conn2),
        Prefix = kraft_util:split_path(Route),
        Module:Method(Conn3, prefix(Path, Prefix), #{})
    catch
        error:undef:StackTrace ->
            return_error_code(undef, Module, Method, StackTrace, 405);
        error:function_clause:StackTrace ->
            return_error_code(function_clause, Module, Method, StackTrace, 404)
    end.

return_error_code(Type, Module, Method, StackTrace, Code) ->
    case StackTrace of
        [{Module, Method, _, _} | _] -> throw(Code);
        _ -> erlang:raise(error, Type, StackTrace)
    end.

method(<<"GET">>) -> get;
method(<<"POST">>) -> post;
method(_) -> throw(501).

prefix([E | L1], [E | L2]) -> prefix(L1, L2);
prefix(L1, _L2) -> L1.
