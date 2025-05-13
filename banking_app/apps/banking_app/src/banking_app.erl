-module(banking_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    banking_app_sup:start_link().

stop(_State) ->
    ok.
