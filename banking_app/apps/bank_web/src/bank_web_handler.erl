-module(bank_web_handler).
-behaviour(cowboy_handler).

-export([init/2, handle_route/5]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    io:format("Method: ~p, Path: ~p~n", [Method, Path]),

    case cowboy_req:binding(user, Req0) of
        undefined ->
            % No user in path — maybe root or users route
            case Path of
                <<"/users">> ->
                    handle_users(Req0);
                _ ->
                    RespBody = jsx:encode(#{message => <<"Welcome to the Erlang Banking App!">>}),
                    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, RespBody, Req0),
                    {ok, Req, State}
            end;
        UserBin when is_binary(UserBin) ->
            HandlerType = State,
            handle_route(HandlerType, UserBin, Method, Req0, State)
    end.

handle_route(deposit, User, <<"POST">>, Req0, _State) ->
    handle_deposit(User, Req0);
handle_route(balance, User, <<"GET">>, Req0, _State) ->
    handle_balance(User, Req0);
handle_route(withdraw, User, <<"POST">>, Req0, _State) ->
    handle_withdraw(User, Req0);
handle_route(_, _User, _Method, Req0, _State) ->
    handle_not_found(Req0).

handle_deposit(User, Req0) ->
    case bank_server:deposit(User, get_amount(Req0)) of
        {ok, NewBalance} ->
            Body = jsx:encode(#{message => <<"Deposit successful">>, balance => NewBalance}),
            Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req0),
            {ok, Req};
        {error, not_found} ->
            respond_not_found(User, Req0)
    end.

handle_balance(User, Req0) ->
    case bank_server:get_balance(User) of
        {ok, Balance} ->
            Body = jsx:encode(#{user => User, balance => Balance}),
            Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req0),
            {ok, Req};
        {error, not_found} ->
            respond_not_found(User, Req0)
    end.

handle_withdraw(User, Req0) ->
    case bank_server:withdraw(User, get_amount(Req0)) of
        {ok, NewBalance} ->
            Body = jsx:encode(#{message => <<"Withdrawal successful">>, balance => NewBalance}),
            Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req0),
            {ok, Req};
        {error, not_found} ->
            respond_not_found(User, Req0);
        {error, insufficient_funds} ->
            Body = jsx:encode(#{error => <<"Insufficient funds">>}),
            Req = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, Body, Req0),
            {ok, Req}
    end.

handle_users(Req0) ->
    Users = bank_server:list_users(),
    Body = jsx:encode(#{users => Users}),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req0),
    {ok, Req1}.

handle_not_found(Req) ->
    Body = jsx:encode(#{error => <<"Route not found">>}),
    Req2 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {ok, Req2}.

respond_not_found(User, Req) ->
    Body = jsx:encode(#{error => <<"User not found">>, user => User}),
    Req2 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {ok, Req2}.

get_amount(Req) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    case string:to_integer(binary_to_list(Body)) of
        {Int, _Rest} -> Int;
        error -> 0
    end.
