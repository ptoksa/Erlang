-module(my_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        {my_counter,
         {my_counter, start_link, []},
         permanent,
         5000,
         worker,
         [my_counter]}
    ],
    {ok, {{one_for_one, 1, 5}, Children}}.
