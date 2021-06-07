-module(kraft_conn).

% API
-export([new/2]).
-export([params/1]).
-export([raw_request/1]).

%--- Types ---------------------------------------------------------------------

-export_type([conn/0]).

-type conn() :: {cowboy_req:req(), any()}.

%--- API -----------------------------------------------------------------------

new(Req, State) -> {Req, State}.

params({Req, _State}) -> maps:get(bindings, Req, #{}).

raw_request({Req, _State}) -> {cowboy, Req}.
