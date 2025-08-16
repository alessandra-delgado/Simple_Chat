open Eio.Std

module Read = Eio.Buf_read
module Write = Eio.Buf_write

let clients = ref []
let client_mutex = Mutex.create ()

let add_client c = 
  Mutex.lock client_mutex;
  clients := c :: !clients;
  Mutex.unlock client_mutex

let remove_client flow = 
  Mutex.lock client_mutex;
  clients := List.filter (fun c -> c != flow) !clients;
  Mutex.unlock client_mutex

let broadcast msg sender = 
  Mutex.lock client_mutex;
  let tmp = !clients in
  Mutex.unlock client_mutex;
  List.iter (fun f -> 
    if f <> sender then 
      try
        Eio.Flow.copy_string (msg ^ "\n") f
      with _ -> remove_client f (*remove recipient if send fails*)
  ) tmp

let handle_client flow _addr =
  Printf.printf "Server: got connection from client\n%!"; (* %! forces a flush *)
  add_client flow;
  let buf_reader = Read.of_flow flow ~max_size:1000 in 
  (try
    while true do
      let msg = Read.line buf_reader in
      traceln "Received: %s" msg;
      broadcast msg flow
    done
  with
  | End_of_file -> traceln "Client disconnected"
  | ex -> traceln "error: %a" Fmt.exn ex
  );
  remove_client flow

let run_server socket =
  Sys.(set_signal sigint (Signal_handle (fun _ -> 
    traceln "Received ctrl+C";
    exit 0)));
  Eio.Net.run_server socket handle_client
    ~on_error:(traceln "Error handling connection: %a" Fmt.exn)

let main ~net ~addr ~clock =
  Switch.run ~name:"main" @@ fun sw ->
    let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
    Fiber.fork_daemon ~sw (fun () -> run_server server);
    traceln "Server listening on %a. Press ctrl+C to stop" Eio.Net.Sockaddr.pp addr;
    Fiber.await_cancel () (*no polling*)
  
let () =
  Eio_main.run @@ fun env -> main
    ~net:(Eio.Stdenv.net env)
    ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))
    ~clock:(Eio.Stdenv.clock env)