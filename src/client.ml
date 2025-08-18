open Eio.Std

module Read = Eio.Buf_read
module Write = Eio.Buf_write

let user_buffer = ref (Buffer.create 100)

let draw_prompt () = 
  Printf.printf "\rsend> ";
  Buffer.output_buffer stdout !user_buffer;
  flush stdout

let handle_char c = 
  match c with
  (*backspace/delete*)
  | '\b' | '\127' ->
    if Buffer.length !user_buffer > 0 then
      (Buffer.truncate !user_buffer (Buffer.length !user_buffer - 1);
      Printf.printf "\b \b%!" (* erases the last char printed *));
    ""
  | '\n' | '\r' ->
    let msg = Buffer.contents !user_buffer in
    Buffer.clear !user_buffer;
    msg
  | c when c >= ' ' && c <= '~' ->
    Buffer.add_char !user_buffer c;
    Printf.printf "%c%!" c;
    ""
  | _ -> ""

let receive buf_reader = 
  traceln "RECEIVE FIBER STARTED -----";
  try 
  while true do 
    let msg = Read.line buf_reader in
    Printf.printf "\r\027[K%!";
    Printf.printf "received: %s\n%!" msg;
    draw_prompt ()
  done
with 
| End_of_file -> Printf.printf "\nServer shut down.\n%!" 
| Eio.Cancel.Cancelled _ -> Printf.printf "'Receive' fiber shut down. Cleaning up...\n%!"
| ex -> (traceln "could not receive correctly: %a" Fmt.exn ex)

let send flow env = 
  (*this function handles user input and message sending..*)
  traceln "SEND FIBER STARTED -----";
  let src = Eio.Stdenv.stdin env in
  try 
    let buf = Cstruct.create 1 in
    draw_prompt ();
    while true do
      let _ = Eio.Flow.single_read src buf in
      let c = Cstruct.get_uint8 buf 0 |> Char.chr in
      let msg = handle_char c in
      flush stdout;
      if msg <> "" then (
        Printf.printf "\r\027[K%!";
        Printf.printf "YOU: %s" msg;
        Printf.printf "\n%!";
        Eio.Flow.copy_string (msg ^ "\n") flow;
        draw_prompt ();
        ) 
  
    done
  with 
  | End_of_file -> Printf.printf "\nServer shut down.\n%!" 
  | Eio.Cancel.Cancelled _ -> Printf.printf "'Send' fiber shut down. Cleaning up...\n%!"
  | ex -> (traceln "could not receive correctly: %a" Fmt.exn ex)

let run_client ~net ~addr ~env =
  Switch.run ~name:"client" @@ fun sw ->
    Printf.printf "Client: connecting to server\n";
    
  let server = Eio.Net.connect ~sw net addr in
  let buf_reader = Read.of_flow server ~max_size:1000 in
  Fiber.first (fun () -> receive buf_reader) (fun () -> send server env)
  
let main ~net ~addr ~env = run_client ~net ~addr ~env

let () = 
  let old_termios = Unix.tcgetattr Unix.stdin  in
  let raw_termios = {old_termios with
    c_icanon = false; (*no line buffering*)
    c_echo = false; (*no echo*)
    c_isig = false;
    c_vmin = 1;
    c_vtime = 0;
  } in
  Unix.tcsetattr Unix.stdin TCSANOW raw_termios; (* sets the standard input with the new settings, also flushes all previous non-treated input *)
  Fun.protect 
    ~finally:(fun () -> Unix.tcsetattr Unix.stdin TCSANOW old_termios; Printf.printf "Terminal restored.\n%!")
    (fun () -> 
      Eio_main.run @@ fun env ->
      main
        ~net:(Eio.Stdenv.net env)
        ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))
        ~env
    )
