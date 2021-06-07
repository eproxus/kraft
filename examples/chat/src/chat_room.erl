-module(chat_room).

-behaviour(kraft_ws_json).

% API
-export([init/1]).
-export([handle/2]).
-export([info/2]).
-export([terminate/2]).

%--- API -----------------------------------------------------------------------

init(State) ->
    join(),
    {[{text, kraft:render(chat, "intro.html", #{user => user()})}], State}.

handle({json, #{chat_message := Message}}, State) ->
    send_message(Message),
    {[], State}.

info({message, From, Message}, State) ->
    Vars = #{message => Message, user => From},
    {[{text, kraft:render(chat, "message.html", Vars)}], State}.

terminate(_Reason, _State) ->
    send_message("[left]").

%--- Internal ------------------------------------------------------------------

join() ->
    pg:join(chat, chat_room, self()),
    send_message(" [joined]").

send_message(Message) ->
    [M ! {message, user(), Message} || M <- pg:get_members(chat, chat_room)].

user() -> io_lib:format("~p", [self()]).
