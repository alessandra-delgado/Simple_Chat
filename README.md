# Simple_Chat
A simple chat made with eio library for OCaml (still in development, and lacks testing).
The server supports multiple client connections.

## How to Run This Project
Navigate to the src directory, and use the `make` command to compile. Execute both the server and client in different bash sessions.
`make clean` will erase both the client and server executables.

If you want to run this on a network, replace the `addr` argument in `client.ml` for 
```ocaml
~addr:(`Tcp ( Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string "your_public_ip"), 8080))
```