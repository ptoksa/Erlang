-module(bank_web_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % No children for now, but structured for future expansion
    {ok, {{one_for_one, 5, 10}, []}}.
