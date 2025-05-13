-module(balance_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    case cowboy_req:binding(user, Req0) of
        undefined ->
            %% No user in path
            {ok, cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"Missing user in URL.\n">>, Req0), State};
        BinUser ->
            User = binary_to_atom(BinUser, utf8),
            case gen_server:call(bank_server, {get_balance, User}) of
                {ok, Balance} ->
                    Body = io_lib:format("Balance for ~p: ~p~n", [User, Balance]),
                    {ok, cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, lists:flatten(Body), Req0), State};
                {error, not_found} ->
                    {ok, cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>}, <<"Account not found\n">>, Req0), State}
            end
    end.
