-module(bank_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    io:format("Starting bank_app...~n", []),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/balance/[...]", bank_handler, []},
            {"/deposit/[...]", bank_handler, []}
        ]}
    ]),

    io:format("Dispatch compiled.~n", []),

    Result = cowboy:start_clear(http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}),

    io:format("Cowboy start_clear result: ~p~n", [Result]),

    case Result of
        {ok, Pid} ->
            io:format("Cowboy started with pid: ~p~n", [Pid]),
            {ok, Pid};
        {error, Reason} ->
            io:format("Failed to start Cowboy: ~p~n", [Reason]),
            {error, Reason}
    end.

stop(_State) ->
    io:format("Stopping bank_app...~n", []),
    ok.
