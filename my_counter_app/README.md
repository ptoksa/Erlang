# my_counter_app

`my_counter_app` is a minimal Erlang application demonstrating a supervised counter process using Erlang/OTP principles. It includes a simple GenServer-style module (`my_counter`) with in-memory state and is structured with a supervision tree.

## 🧱 Structure

- `my_app.erl` — Application entry point
- `my_sup.erl` — Supervisor module
- `my_counter.erl` — GenServer-like counter module

## 🚀 Getting Started

### Prerequisites

- Erlang/OTP 25 or newer
- [rebar3](https://www.rebar3.org/) (build tool)

### Clone and Compile

```bash
git clone <your-repo-url>
cd my_counter_app
rebar3 compile
```
## Run the App in Shell
```bash
rebar3 shell
```
You should see:
```bash
1> my_counter:increment().
ok
2> my_counter:get().
1
```
## 🛠️ Modules
### `my_counter`
Basic API for incrementing and reading a counter.
```bash
my_counter:get().         % returns current count
my_counter:increment().   % increases count by 1
```
### `my_sup`
Defines a one-for-one supervisor. Right now it supervises the my_counter process.
### `my_app`
Starts the supervision tree when the app boots.
## 🧪 Run the unit tests
This project includes EUnit tests defined in `my_counter.erl`.

To run the tests:
```bash
rebar3 eunit
```
Expected output:
```bash
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling my_counter_app
===> Performing EUnit tests...
......
Finished in 0.096 seconds
6 tests, 0 failures
```

## 📁 Project Layout (Typical)
```bash
my_counter_app/
├── src/
│   ├── my_app.erl
│   ├── my_sup.erl
│   ├── my_counter.erl
│   └── my_counter_app.app.src
├── rebar.config
└── README.md
```