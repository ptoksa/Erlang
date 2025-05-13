-module(my_crud_app).
-export([start/0]).

start() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/users", my_crud_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, 100,
        #{port => 8080},
        #{env => #{dispatch => Dispatch}}),
    ok.
