-module(kraft_controller).

-type conn() :: {cowboy_req:req(), any()}.

-callback init(conn()) -> ok.

% API
-export([init/1]).

%--- API -----------------------------------------------------------------------

init(_) ->
    ok.
