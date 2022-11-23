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
        {"/error", module_does_not_exist, #{}},
        {"/missing_template", ?MODULE, #{}}
    ]),
    {ok, self(), Ref}.

stop(Ref) -> kraft:stop(Ref).

init(Conn, _Params, _State) -> kraft:render(Conn, "missing", #{}).
