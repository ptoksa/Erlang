-module(hello_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", hello_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, 100,
        #{port => 8080},
        #{env => #{dispatch => Dispatch}}
    ),
    % Specify the supervisor strategy and children (empty for now)
    {ok, {{one_for_one, 10, 10}, []}}.
