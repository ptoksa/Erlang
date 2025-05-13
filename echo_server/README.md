# Erlang Echo Server

This is a simple TCP echo server written in Erlang. It listens on a given port, accepts incoming connections, and echoes back any received data.

## Features

- Accepts TCP connections
- Echoes received binary data back to the client
- Handles each client connection in a separate process
- Gracefully handles client disconnections

## Requirements

- Erlang/OTP installed on your system

## Usage

### 1. Compile the Module

```bash
erlc echo_server.erl
```
### 2. Start the Erlang Shell
```bash
erl
```
### 3. Run the Server
In the Erlang shell:
```bash
c(echo_server).
echo_server:start(1234).
```
Replace `1234` with the port number you want the server to listen on.
### 4. Connect to the Server
VYou can use `telnet`, `nc`, or a custom TCP client to connect:
```bash
nc localhost 1234
```
Type any message and it will be echoed back by the server.
## Example Output
```bash
> nc localhost 1234
hello
hello
```
On the server side, you'll see:
```bash
Listening on port 1234
Received: <<"hello">>
```
