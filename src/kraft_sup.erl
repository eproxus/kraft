-module(kraft_sup).

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
        worker(kraft_template),
        supervisor(kraft_instance_sup)
    ],
    {ok, {#{}, Children}}.

%--- Internal ------------------------------------------------------------------

worker(Module) ->
    #{id => Module, start => {Module, start_link, []}}.

supervisor(Module) ->
    #{id => Module, type => supervisor, start => {Module, start_link, []}}.
