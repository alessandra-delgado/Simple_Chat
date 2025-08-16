open Eio.Std

let run_client ~net ~addr =
  Switch.run ~name:"client" @@ fun sw ->
  Printf.printf "Client: connecting to server\n";
  
  let flow = Eio.Net.connect ~sw net addr in
  Printf.printf "Client: received %S\n" (Eio.Flow.read_all flow)

let main ~net ~addr = run_client ~net ~addr

let () = Eio_main.run @@ fun env -> main
    ~net:(Eio.Stdenv.net env)
    ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))