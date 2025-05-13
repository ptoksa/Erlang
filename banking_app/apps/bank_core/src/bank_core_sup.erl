%%%-------------------------------------------------------------------
%% @doc bank_core top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bank_core_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    ChildSpecs = [
        #{id => bank_server,
          start => {bank_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [bank_server]}
    ],

    {ok, {SupFlags, ChildSpecs}}.
