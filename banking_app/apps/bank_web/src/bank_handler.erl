-module(bank_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, _Opts) ->
    {Method, PathBin} = {cowboy_req:method(Req0), cowboy_req:path(Req0)},
    Path = binary_to_list(PathBin),

    case {Method, Path} of
        %% Serve index.html
        {<<"GET">>, "/"} ->
            case file:read_file("priv/static/index.html") of
                {ok, Bin} ->
                    {ok, cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Bin, Req0), undefined};
                {error, _} ->
                    {ok, cowboy_req:reply(404, #{}, <<"Not Found">>, Req0), undefined}
            end;

        %% Serve static files from /static/
        {<<"GET">>, _} ->
            case string:prefix(Path, "/static/") of
                true ->
                    File = string:replace(Path, "/static/", "", [{return, list}]),
                    FullPath = filename:join(["priv/static", File]),
                    case file:read_file(FullPath) of
                        {ok, Bin} ->
                            Mime = mime_type(File),
                            {ok, cowboy_req:reply(200, #{<<"content-type">> => Mime}, Bin, Req0), undefined};
                        {error, _} ->
                            {ok, cowboy_req:reply(404, #{}, <<"Not Found">>, Req0), undefined}
                    end;
                false ->
                    Response = route(Method, PathBin, Req0),
                    {ok, cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req0), undefined}
            end;

        %% Other methods (e.g., POST)
        _ ->
            Response = route(Method, PathBin, Req0),
            {ok, cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req0), undefined}
    end.

%% API Routing

route(<<"GET">>, <<"/balance/", Name/binary>>, _Req) ->
    case bank_server:get_balance(binary_to_atom(Name, utf8)) of
        {balance, Bal} -> encode({Name, balance, Bal});
        {error, _} -> encode({error, not_found})
    end;

route(<<"POST">>, <<"/deposit/", Name/binary>>, Req) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    case binary:split(Body, <<"=">>) of
        [<<"amount">>, AmountBin] ->
            Amount = binary_to_integer(AmountBin),
            case bank_server:deposit(binary_to_atom(Name, utf8), Amount) of
                {ok, NewBal} -> encode({Name, new_balance, NewBal});
                {error, _} -> encode({error, not_found})
            end;
        _ ->
            encode({error, invalid_format})
    end;

route(_, _, _) ->
    encode({error, unsupported}).

%% JSON encoder

encode(Term) ->
    jsx:encode(Term).

%% MIME type helper

mime_type(File) ->
    case filename:extension(File) of
        ".html" -> <<"text/html">>;
        ".css"  -> <<"text/css">>;
        ".js"   -> <<"application/javascript">>;
        ".json" -> <<"application/json">>;
        ".png"  -> <<"image/png">>;
        ".jpg"  -> <<"image/jpeg">>;
        ".ico"  -> <<"image/x-icon">>;
        _       -> <<"application/octet-stream">>
    end.
