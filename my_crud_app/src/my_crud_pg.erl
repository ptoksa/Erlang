-module(my_crud_pg).
-export([create_user/1, get_users/0]).

create_user(Name) ->
    {ok, C} = epgsql:connect("localhost", "postgres", "postgres", [{database, "testdb"}]),
    _ = epgsql:equery(C, "INSERT INTO users (name) VALUES ($1)", [Name]),
    epgsql:close(C),
    ok.

get_users() ->
    {ok, C} = epgsql:connect("localhost", "postgres", "postgres", [{database, "testdb"}]),
    {ok, _, Rows} = epgsql:squery(C, "SELECT id, name FROM users"),
    epgsql:close(C),
    {ok, Rows}.
