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
    {ok, {#{}, [child(kraft_template)]}}.

child(Module) -> child(Module, []).

child(Module, Args) -> #{id => Module, start => {Module, start_link, Args}}.
