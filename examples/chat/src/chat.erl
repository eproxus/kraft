-module(chat).

-behaviour(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_StartType, _StartArgs) ->
    kraft:start(#{port => 8093}, [
        {"/chatroom", {ws, chat_room}, #{}, #{type => json}},
        {"/", kraft_static, #{}}
    ]),
    chat_sup:start_link().

stop(_State) ->
    kraft:stop().
