open Eio.Std

module Read = Eio.Buf_read
module Write = Eio.Buf_write

let user_buffer = Buffer.create 100

let handle_char c = 
  match c with
  (*backspace/delete*)
  | '\b' | '\127' ->
    if Buffer.length user_buffer > 0 then
      (Buffer.truncate user_buffer (Buffer.length user_buffer - 1);
      Printf.printf "\b \b%!"; (* erases the last char printed *)
      "") 
  | '\n' | '\r' ->
    let msg = Buffer.contents user_buffer in
    Buffer.clear user_buffer;
    msg
  | c when c >= ' ' && c <= '~' ->
    Buffer.add_char user_buffer c;
    Printf.printf "%c%!" c;
    ""
  | _ -> ""

let receive buf_reader = 
  traceln "RECEIVE FIBER STARTED -----";
  try 
  while true do 
    let msg = Read.line buf_reader in
    Printf.printf "\r\027[Kreceived:%s\nsend a message> %!" msg; (* also clears what was being typed. i may have to define my own buffer to display what one is currently typing for this case*)
  done
with ex -> (traceln "could not receive correctly: %a" Fmt.exn ex; exit 0)

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
  with ex -> (traceln "could not send correctly: %a" Fmt.exn ex; exit 0)

let run_client ~net ~addr ~env =
  Switch.run ~name:"client" @@ fun sw ->
    Printf.printf "Client: connecting to server\n";
    
  let server = Eio.Net.connect ~sw net addr in
  let buf_reader = Read.of_flow server ~max_size:1000 in
  Fiber.fork ~sw (fun () -> receive buf_reader);
  Fiber.fork ~sw (fun () -> send server env)

let main ~net ~addr ~env= run_client ~net ~addr ~env

let () = Eio_main.run @@ fun env -> main
    ~net:(Eio.Stdenv.net env)
    ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))
    ~env