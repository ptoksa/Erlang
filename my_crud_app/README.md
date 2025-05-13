# my_crud_app

A simple CRUD web API built with **Erlang**, using **Cowboy** as the HTTP server and **PostgreSQL** as the database. The API supports fetching and inserting users via HTTP endpoints.

## Features

- Built with [Cowboy](https://ninenines.eu/docs/en/cowboy/) web server.
- Connects to PostgreSQL using [epgsql](https://github.com/epgsql/epgsql).
- JSON responses generated using [jsx](https://github.com/talentdeficit/jsx).
- Demonstrates basic CRUD operations in Erlang.

---

## Requirements

- Erlang/OTP 25 or later
- PostgreSQL (with a `testdb` database)
- [rebar3](https://www.rebar3.org/) build tool

---

## Getting Started

### 1. Clone the repository

```bash
git clone https://github.com/yourname/my_crud_app.git
cd my_crud_app
```
### 2. Setup PostgreSQL
Create the `testdb` database and user:
```bash
-- In psql or your PostgreSQL client
CREATE DATABASE testdb;
CREATE USER petri WITH PASSWORD 'qwerty';
GRANT ALL PRIVILEGES ON DATABASE testdb TO petri;

-- Create the table
\c testdb
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL
);
```
### 3. Install dependencies and compile
```bash
rebar3 compile
```
### 4. Start the app
```bash
rebar3 shell
```
Then in the Erlang shell:
```bash
application:ensure_all_started(ranch).
application:ensure_all_started(cowboy).
Dispatch = cowboy_router:compile([{'_', [{"/users", my_crud_handler, []}]}]).
cowboy:start_clear(http_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}).
```
## Usage
### View all users (browser or curl)
You can either:

- Open your browser and go to:
http://localhost:8080/users

Or:
```bash
curl http://localhost:8080/users
```
### Add a user (from shell)
```bash
my_crud_pg:create_user(<<"Alice">>).
```
Then refresh the `/users` endpoint to see the updated list.
## Project Structure
```bash
my_crud_app/
├── rebar.config
├── README.md
└── src/
    ├── my_crud_app.app.src
    ├── my_crud_handler.erl       % Cowboy HTTP handler
    └── my_crud_pg.erl            % PostgreSQL interaction logic
```
## Author
Petri — 2025