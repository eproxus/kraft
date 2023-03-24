-module(index).

-behaviour(application).
-behaviour(kraft_controller).

% Callbacks
-export([start/2]).
-export([stop/1]).
-export([init/3]).

%--- Callbacks -----------------------------------------------------------------

start(_StartType, _StartArgs) ->
    Ref = kraft:start(#{port => 8090}, [
        {"/", kraft_static, #{file => "index.html"}},
        {"/error", ?MODULE, #{error => undef}},
        {"/missing_template", ?MODULE, #{error => missing_template}}
    ]),
    {ok, self(), Ref}.

stop(Ref) -> kraft:stop(Ref).

init(_Conn, _Params, #{error := undef}) ->
    module_does_not_exist:function_does_not_exist(#{
        big => #{complex => #{argument => {with, many, [terms, inside]}}}
    });
init(Conn, _Params, #{error := missing_template}) ->
    kraft:render(Conn, "missing", #{}).
