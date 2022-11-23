-module(kraft_instance_sup).

-behaviour(supervisor).

% API
-ignore_xref([{?MODULE, start_link, 0}]).
-export([start_link/0]).
-export([start_instance/1]).
-export([stop_instance/1]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_instance(Params) -> supervisor:start_child(?MODULE, [Params]).

stop_instance(ID) -> supervisor:terminate_child(?MODULE, ID).

%--- Callbacks -----------------------------------------------------------------

init([]) ->
    Template = #{id => instance, start => {kraft_instance, start_link, []}},
    {ok, {#{strategy => simple_one_for_one}, [Template]}}.
