-module(kraft_controller).

-type conn() :: {cowboy_req:req(), any()}.
-type params() :: map:map().

-callback init(conn(), params()) -> ok.
