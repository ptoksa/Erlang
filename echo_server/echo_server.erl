-module(echo_server).
-export([start/1, accept/1]).

start(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    io:format("Listening on port ~p~n", [Port]),
    accept(ListenSocket).

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_client(Socket) end),
    accept(ListenSocket).

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Received: ~p~n", [Data]),
            gen_tcp:send(Socket, Data),
            handle_client(Socket);
        {error, closed} ->
            io:format("Client disconnected~n"),
            ok
    end.
