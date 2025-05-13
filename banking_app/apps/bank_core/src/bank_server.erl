-module(bank_server).
-behaviour(gen_server).

%% API
-export([start/0, start_link/0]).
-export([
    create_account/1,
    deposit/2,
    withdraw/2,
    get_balance/1,
    list_users/0,
    delete_account/1,
    rename_account/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%% API Functions

start() ->
    gen_server:start({local, bank_server}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, bank_server}, ?MODULE, [], []).

create_account(Name) ->
    gen_server:call(bank_server, {create_account, Name}).

deposit(Name, Amount) ->
    gen_server:call(bank_server, {deposit, Name, Amount}).

withdraw(Name, Amount) ->
    gen_server:call(bank_server, {withdraw, Name, Amount}).

get_balance(Name) ->
    gen_server:call(bank_server, {get_balance, Name}).

list_users() ->
    gen_server:call(bank_server, list_users_request).

delete_account(Name) ->
    gen_server:call(bank_server, {delete_account, Name}).

rename_account(OldName, NewName) ->
    gen_server:call(bank_server, {rename_account, OldName, NewName}).

%%% gen_server Callbacks

init([]) ->
    {ok, #{}}.

handle_call({create_account, Name}, _From, Accounts) ->
    case maps:is_key(Name, Accounts) of
        true ->
            {reply, {error, exists}, Accounts};
        false ->
            Pid = bank_account:start(),
            {reply, {ok, Pid}, maps:put(Name, Pid, Accounts)}
    end;

handle_call({deposit, Name, Amount}, _From, Accounts) ->
    case maps:find(Name, Accounts) of
        {ok, Pid} ->
            Pid ! {deposit, Amount, self()},
            receive
                {ok, NewBalance} ->
                    io:format("[~p] Deposit: ~p → New balance: ~p~n", [Pid, Amount, NewBalance]),
                    {reply, {ok, NewBalance}, Accounts}
            end;
        error ->
            {reply, {error, not_found}, Accounts}
    end;

handle_call({withdraw, Name, Amount}, _From, Accounts) ->
    case maps:find(Name, Accounts) of
        {ok, Pid} ->
            Pid ! {withdraw, Amount, self()},
            receive
                Reply ->
                    case Reply of
                        {ok, NewBalance} ->
                            io:format("[~p] Withdraw: ~p → New balance: ~p~n", [Pid, Amount, NewBalance]),
                            {reply, {ok, NewBalance}, Accounts};
                        Error ->
                            {reply, Error, Accounts}
                    end
            end;
        error ->
            {reply, {error, not_found}, Accounts}
    end;

handle_call({get_balance, Name}, _From, Accounts) ->
    case maps:find(Name, Accounts) of
        {ok, Pid} ->
            Pid ! {get_balance, self()},
            receive
                {balance, Bal} ->
                    io:format("[~p] Balance inquiry: ~p~n", [Pid, Bal]),
                    {reply, {ok, Bal}, Accounts}
            end;
        error ->
            {reply, {error, not_found}, Accounts}
    end;

handle_call(list_users_request, _From, Accounts) ->
    {reply, [Name || {Name, _Pid} <- maps:to_list(Accounts)], Accounts};

handle_call({delete_account, Name}, _From, Accounts) ->
    case maps:take(Name, Accounts) of
        {Pid, NewAccounts} ->
            exit(Pid, shutdown),
            {reply, ok, NewAccounts};
        error ->
            {reply, {error, not_found}, Accounts}
    end;

handle_call({rename_account, OldName, NewName}, _From, Accounts) ->
    case {maps:find(OldName, Accounts), maps:is_key(NewName, Accounts)} of
        {{ok, Pid}, false} ->
            NewAccounts = maps:put(NewName, Pid, maps:remove(OldName, Accounts)),
            {reply, ok, NewAccounts};
        {{ok, _}, true} ->
            {reply, {error, new_name_exists}, Accounts};
        {error, _} ->
            {reply, {error, not_found}, Accounts}
    end.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
