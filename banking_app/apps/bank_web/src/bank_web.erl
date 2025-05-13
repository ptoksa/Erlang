-module(bank_web).
-export([init/2, call/3]).

init(_Transport, _Opts) ->
    {ok, #{}}.
    
start() ->
    bank_web_sup:start_link().

call(Req0, _Env, State) ->
    Method = cowboy_req:method(Req0),
    PathInfo = cowboy_req:path_info(Req0),
    case {Method, PathInfo} of
        {'GET', [<<"balance">>, NameBin]} ->
            Name = binary_to_atom(NameBin, utf8),
            case bank_server:get_balance(Name) of
                {ok, Balance} ->
                    Body = io_lib:format("Balance for ~s: ~p", [atom_to_list(Name), Balance]),
                    {ok, Resp} = cowboy_req:reply(200, #{}, lists:flatten(Body), Req0),
                    {ok, Resp, State};
                {error, not_found} ->
                    {ok, Resp} = cowboy_req:reply(404, #{}, "Account not found", Req0),
                    {ok, Resp, State}
            end;

        {'POST', [<<"deposit">>, NameBin]} ->
            Name = binary_to_atom(NameBin, utf8),
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            case extract_amount(Body) of
                {ok, Amount} ->
                    case bank_server:deposit(Name, Amount) of
                        {ok, NewBalance} ->
                            Msg = io_lib:format("Deposited ~p. New balance: ~p", [Amount, NewBalance]),
                            {ok, Resp} = cowboy_req:reply(200, #{}, lists:flatten(Msg), Req1),
                            {ok, Resp, State};
                        _ ->
                            {ok, Resp} = cowboy_req:reply(400, #{}, "Deposit failed", Req1),
                            {ok, Resp, State}
                    end;
                error ->
                    {ok, Resp} = cowboy_req:reply(400, #{}, "Invalid amount", Req1),
                    {ok, Resp, State}
            end;

        {'POST', [<<"withdraw">>, NameBin]} ->
            Name = binary_to_atom(NameBin, utf8),
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            case extract_amount(Body) of
                {ok, Amount} ->
                    case bank_server:withdraw(Name, Amount) of
                        {ok, NewBalance} ->
                            Msg = io_lib:format("Withdrew ~p. New balance: ~p", [Amount, NewBalance]),
                            {ok, Resp} = cowboy_req:reply(200, #{}, lists:flatten(Msg), Req1),
                            {ok, Resp, State};
                        {error, insufficient_funds} ->
                            {ok, Resp} = cowboy_req:reply(400, #{}, "Insufficient funds", Req1),
                            {ok, Resp, State};
                        _ ->
                            {ok, Resp} = cowboy_req:reply(400, #{}, "Withdrawal failed", Req1),
                            {ok, Resp, State}
                    end;
                error ->
                    {ok, Resp} = cowboy_req:reply(400, #{}, "Invalid amount", Req1),
                    {ok, Resp, State}
            end;

        _ ->
            {ok, Resp} = cowboy_req:reply(404, #{}, "Not found", Req0),
            {ok, Resp, State}
    end.

extract_amount(BodyBin) ->
    %% Parses the body as "amount=50"
    case binary:split(BodyBin, <<"=">>, [global]) of
        [<<"amount">>, AmountBin] ->
            case string:to_integer(binary_to_list(AmountBin)) of
                {Int, _Rest} -> {ok, Int};
                _ -> error
            end;
        _ ->
            error
    end.
