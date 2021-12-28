-module(index).

-behaviour(application).
-behaviour(kraft_controller).

% Callbacks
-export([start/2]).
-export([stop/1]).
-export([init/3]).

%--- Callbacks -----------------------------------------------------------------

start(_StartType, _StartArgs) ->
    kraft:start(#{port => 8090}, [
        {"/", kraft_static, #{file => "index.html"}},
        {"/error", module_does_not_exist, #{}},
        {"/missing_template", ?MODULE, #{}}
    ]),
    {ok, self()}.

stop(_State) ->
    kraft:stop().

init(Conn, _Params, _State) ->
    {200, #{}, kraft:render(Conn, "missing", #{})}.
