-module(bank_account).
-export([start/0, loop/1]).

start() ->
    spawn(?MODULE, loop, [0]).

loop(Balance) ->
    receive
        {deposit, Amount, From} when is_integer(Amount), Amount > 0 ->
            NewBalance = Balance + Amount,
            io:format("[~p] Deposit: ~p → New balance: ~p~n", [self(), Amount, NewBalance]),
            From ! {ok, NewBalance},
            loop(NewBalance);

        {withdraw, Amount, From} when is_integer(Amount), Amount > 0 ->
            if
                Amount =< Balance ->
                    NewBalance = Balance - Amount,
                    io:format("[~p] Withdraw: ~p → New balance: ~p~n", [self(), Amount, NewBalance]),
                    From ! {ok, NewBalance},
                    loop(NewBalance);
                true ->
                    io:format("[~p] Withdraw failed: insufficient funds~n", [self()]),
                    From ! {error, insufficient_funds},
                    loop(Balance)
            end;

        {get_balance, From} ->
            io:format("[~p] Balance inquiry: ~p~n", [self(), Balance]),
            From ! {balance, Balance},
            loop(Balance);

        _Other ->
            io:format("[~p] Unknown message received: ~p~n", [self(), _Other]),
            loop(Balance)
    end.
