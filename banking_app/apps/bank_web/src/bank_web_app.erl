-module(bank_web_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Start the supervision tree first
    {ok, Pid} = bank_web_sup:start_link(),

    % Compile routes from router module
    Dispatch = bank_web_router:routes(),

    % Start Cowboy with compiled routes and port 8081
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8081}],
        #{env => #{dispatch => Dispatch}}),

    {ok, Pid}.

stop(_State) ->
    ok.
