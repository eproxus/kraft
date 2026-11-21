-module(kraft_app).

-moduledoc """
Kraft public API.

This module is the entry point for the Kraft application. It starts the
supervisor and stops the application.
""".

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kraft_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
