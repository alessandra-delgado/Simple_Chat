open Eio.Std

let handle_client flow _addr =
  Printf.printf "Server: got connection from client\n%!"; (* %! forces flush *)
  Eio.Flow.copy_string "Hello from server" flow

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
    traceln "Server listening on %a. Press ctrl+C to stop\n%!" Eio.Net.Sockaddr.pp addr;
    Fiber.await_cancel () (*no polling*)
  
let () =
    
  Eio_main.run @@ fun env -> main
    ~net:(Eio.Stdenv.net env)
    ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))
    ~clock:(Eio.Stdenv.clock env)