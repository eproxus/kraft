-module(index).

-behaviour(application).
-behaviour(kraft_controller).

% Callbacks
-export([start/2]).
-export([stop/1]).
-export([init/2]).

%--- Callbacks -----------------------------------------------------------------

start(_StartType, _StartArgs) ->
    Ref = kraft:start(#{
        #{scheme => http, port => 8090} => #{
            routes => [
                {"/", kraft_static, #{file => "index.html"}},
                {"/error", ?MODULE, #{error => undef}},
                {"/missing_template", ?MODULE, #{error => missing_template}}
            ]
        }
    }),
    {ok, self(), Ref}.

stop(Ref) -> kraft:stop(Ref).

init(_Conn, #{error := undef}) ->
    StackTrace =
        try
            throw(foo)
        catch
            throw:foo:ST ->
                Module = module_does_not_exist,
                Function = function_does_not_exist,
                Arg = #{
                    big => #{
                        complex => #{argument => {with, many, [terms, inside]}}
                    }
                },
                [{Module, Function, [Arg], []} | ST]
        end,
    erlang:raise(exit, undef, StackTrace);
init(Conn, #{error := missing_template}) ->
    {respond, Conn, {template, "missing", #{}}}.
