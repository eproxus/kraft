-module(chat_sup).

-behaviour(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%--- Callbacks -----------------------------------------------------------------

init([]) ->
    Children = [
        #{id => chat, start => {pg, start_link, [chat]}}
    ],
    {ok, {#{}, Children}}.
