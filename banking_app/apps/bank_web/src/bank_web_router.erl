-module(bank_web_router).
-export([routes/0]).

routes() ->
    cowboy_router:compile([
        {'_', [
            {"/", bank_web_handler, home},
            {"/deposit/:user", bank_web_handler, deposit},
            {"/balance/:user", bank_web_handler, balance},
            {"/withdraw/:user", bank_web_handler, withdraw},
            {"/users", bank_web_handler, users}
        ]}
    ]).
