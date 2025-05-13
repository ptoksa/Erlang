# hello_web

A minimal Erlang web application using [Cowboy](https://ninenines.eu/docs/en/cowboy/) as the HTTP server.

This project demonstrates how to serve a simple "Hello, World!" response from a web handler using Cowboy.

## Prerequisites

- Erlang/OTP 25 (or compatible version)
- `rebar3` build tool

## Project Structure
```bash
hello_web/
├── src/
│ ├── hello_app.erl (optional)
│ └── hello_handler.erl <-- Cowboy request handler
├── rebar.config <-- Dependency & project config
└── ...
```
## Installation

1. Clone or download this repository.
2. Ensure dependencies are installed:
   ```bash
   rebar3 get-deps

## Building the Project
To compile the code:
```bash
rebar3 compile
```
## Running the Server
Launch the shell and start the HTTP server:
```bash
rebar3 shell
```
In the Erlang shell:
```bash
application:start(cowlib),
application:start(ranch),
application:start(cowboy).

Dispatch = cowboy_router:compile([
    {'_', [{"/", hello_handler, []}]}
]),

{ok, _} = cowboy:start_clear(http_listener,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}).
```
Once the server is running, visit http://localhost:8080 in your browser. You should see:
```bash
Hello, World!
```
## Stopping the Server
To stop the Cowboy listener:
```bash
cowboy:stop_listener(http_listener).
```
Or to exit the entire Erlang shell:
```bash
init:stop().
```
## `hello_handler` Description
The `hello_handler.erl` module implements the`cowboy_handler` behavior and returns a plain text HTTP response:
```bash
-module(hello_handler).
-behaviour(cowboy_handler).

-export([init/2, handle/2, terminate/3]).

init(Req, Opts) ->
    {ok, Req, Opts}.

handle(Req, State) ->
    ResponseBody = <<"Hello, World!">>,
    Headers = [{<<"content-type">>, <<"text/plain">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, ResponseBody, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
```
