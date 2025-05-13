-module(bank_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req, _Opts) ->
    {Method, Path} = {cowboy_req:method(Req), cowboy_req:path(Req)},
    Response = route(Method, Path, Req),
    {ok, cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req), undefined}.

route(<<"GET">>, <<"/balance/", Name/binary>>, _Req) ->
    case bank_server:get_balance(binary_to_atom(Name, utf8)) of
        {balance, Bal} -> encode({Name, balance, Bal});
        {error, _} -> encode({error, not_found})
    end;

route(<<"POST">>, <<"/deposit/", Name/binary>>, Req) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    %% Parse "amount=50"
    case binary:split(Body, <<"=">>) of
        [<<"amount">>, AmountBin] ->
            Amount = binary_to_integer(AmountBin),
            case bank_server:deposit(binary_to_atom(Name, utf8), Amount) of
                {ok, NewBal} -> encode({Name, new_balance, NewBal});
                {error, _} -> encode({error, not_found})
            end;
        _ ->
            encode({error, invalid_format})
    end;

route(_, _, _) ->
    encode({error, unsupported}).

encode(Term) ->
    jsx:encode(Term).
