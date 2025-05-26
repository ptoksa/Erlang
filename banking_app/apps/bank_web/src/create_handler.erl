-module(create_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    case cowboy_req:binding(user, Req0) of
        undefined ->
            %% No user in path
            {ok, cowboy_req:reply(400,
                #{<<"content-type">> => <<"text/plain">>},
                <<"Missing user in URL.\n">>, Req0), State};
        BinUser ->
            User = binary_to_atom(BinUser, utf8),
            case gen_server:call(bank_server, {create_account, User}) of
                ok ->
                    {ok, cowboy_req:reply(201,
                        #{<<"content-type">> => <<"text/plain">>},
                        <<"Account created\n">>, Req0), State};
                {error, already_exists} ->
                    {ok, cowboy_req:reply(409,
                        #{<<"content-type">> => <<"text/plain">>},
                        <<"Account already exists\n">>, Req0), State}
            end
    end.
