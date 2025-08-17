open Eio.Std

module Read = Eio.Buf_read
module Write = Eio.Buf_write

let receive buf_reader = 
  traceln "RECEIVE FIBER STARTED -----";
  try 
  while true do 
    let msg = Read.line buf_reader in
    Printf.printf "received: %s\n%!" msg;
  done
with ex -> (traceln "could not receive correctly: %a" Fmt.exn ex)

let send flow env = 
  traceln "SEND FIBER STARTED -----";
  try 
    let stdin_flow = Eio.Stdenv.stdin env in
    let stdin_reader = Read.of_flow stdin_flow ~max_size:1000 in
    while true do
      Printf.printf "send a message> %!";
      let input = Read.line stdin_reader in
      traceln "You typed: %s" input;
      Eio.Flow.copy_string (input ^ "\n") flow
    done
  with ex -> (traceln "could not send correctly: %a" Fmt.exn ex)

let run_client ~net ~addr ~env =
  Switch.run ~name:"client" @@ fun sw ->
    Printf.printf "Client: connecting to server\n";
    
  let server = Eio.Net.connect ~sw net addr in
  let buf_reader = Read.of_flow server ~max_size:1000 in
  Fiber.fork ~sw (fun () -> receive buf_reader);
  Fiber.fork ~sw (fun () -> send server env);
  Fiber.await_cancel ()
  

let main ~net ~addr ~env= run_client ~net ~addr ~env

let () = Eio_main.run @@ fun env -> main
    ~net:(Eio.Stdenv.net env)
    ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))
    ~env