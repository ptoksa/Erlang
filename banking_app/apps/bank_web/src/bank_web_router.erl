-module(bank_web_router).
-export([routes/0]).

routes() ->
    cowboy_router:compile([
        {'_', [
            {"/create/:user", create_handler, []},
            {"/balance/:user", balance_handler, []},
            %% fallback handler for unknown paths (optional)
            {"/[...]", bank_handler, []}
        ]}
    ]).
