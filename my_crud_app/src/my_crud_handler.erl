%% my_crud_handler.erl
-module(my_crud_handler).
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req, State) ->
    Method = cowboy_req:method(Req),
    case Method of
        <<"GET">> ->
            handle_get(Req, State);
        <<"POST">> ->
            handle_post(Req, State);
        _ ->
            {cowboy_req:reply(405, #{<<"content-type">> => <<"text/plain">>}, <<"Method Not Allowed">>, Req), State}
    end.

handle_get(Req, State) ->
    case my_crud_pg:get_users() of
        {ok, Rows} ->
            Json = format_rows(Rows),
            {cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req), State};
        {error, _} ->
            {cowboy_req:reply(500, #{}, <<"Internal Server Error">>, Req), State}
    end.

handle_post(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case jsx:decode(Body, [return_maps]) of
        #{<<"name">> := NameBin} when is_binary(NameBin) ->
            case my_crud_pg:create_user(NameBin) of
                ok ->
                    {cowboy_req:reply(201, #{<<"content-type">> => <<"text/plain">>}, <<"User created">>, Req2), State};
                {error, _} ->
                    {cowboy_req:reply(500, #{}, <<"Insert Failed">>, Req2), State}
            end;
        _ ->
            {cowboy_req:reply(400, #{}, <<"Bad Request">>, Req2), State}
    end.

terminate(_, _, _) ->
    ok.

format_rows(Rows) ->
    %% Convert [{Id, Name}, ...] to JSON list
    JsonList = lists:map(fun({Id, Name}) ->
        #{<<"id">> => Id, <<"name">> => Name}
    end, Rows),
    jsx:encode(JsonList).
