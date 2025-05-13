-module(hello_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, _State) ->
    Reply = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Hello, World!">>,
        Req),
    {ok, Reply, undefined}.
