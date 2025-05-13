-module(my_counter).
-export([start_link/0, get/0, increment/0]).
-include_lib("eunit/include/eunit.hrl").

%% Simple counter using registered process and state
start_link() ->
    register(?MODULE, spawn(fun loop/0)),
    {ok, self()}.

loop() ->
    loop(0).

loop(Count) ->
    receive
        {increment, From} ->
            From ! ok,
            loop(Count + 1);
        {get, From} ->
            From ! Count,
            loop(Count);
        stop ->
            ok
    end.

get() ->
    ?MODULE ! {get, self()},
    receive
        Count -> Count
    end.

increment() ->
    ?MODULE ! {increment, self()},
    receive
        ok -> ok
    end.

%%% ---------------------
%%% EUnit Tests
%%% ---------------------

my_counter_test_() ->
    {setup,
     fun() ->
         {ok, _Pid} = my_counter:start_link(),
         ok
     end,
     fun(_Context) ->
         my_counter ! stop
     end,
     fun(_Context) ->
         [
             ?_assertEqual(0, my_counter:get()),
             ?_assertEqual(ok, my_counter:increment()),
             ?_assertEqual(1, my_counter:get()),
             ?_assertEqual(ok, my_counter:increment()),
             ?_assertEqual(ok, my_counter:increment()),
             ?_assertEqual(3, my_counter:get())
         ]
     end}.
