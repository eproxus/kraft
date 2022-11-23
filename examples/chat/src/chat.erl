-module(chat).

-behaviour(application).
-behaviour(kraft_controller).

% Callbacks
-export([start/2]).
-export([stop/1]).
-export([init/3]).

%--- Callbacks -----------------------------------------------------------------

start(_StartType, _StartArgs) ->
    Ref = kraft:start(#{port => 8093}, [
        {"/chatroom", {ws, chat_room}, #{}, #{type => json}},
        {"/", ?MODULE, #{}},
        {"/", kraft_static, #{}}
    ]),
    {ok, Pid} = chat_sup:start_link(),
    {ok, Pid, Ref}.

stop(Ref) -> kraft:stop(Ref).

init(Conn, _Params, _State) -> kraft:render(Conn, "index.html", #{}).
