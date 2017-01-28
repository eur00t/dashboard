open Core.Std;;
open Async.Std;;
open Log.Global;;

open Websocket_async;;

let id = ref 0;;

type socket = {
    sender_write: Frame.t Pipe.Writer.t;
    receiver_read: Frame.t Pipe.Reader.t;
};;


type t = {
    id: int ref;
    sockets: socket Int.Table.t;
};;

let send_all_frame { sockets } msg =
    let sockets = Int.Table.to_alist sockets in
    Deferred.all_unit
        (List.map sockets (fun (_, { sender_write }) ->
        Pipe.write sender_write msg));;

let send_all t msg =
    send_all_frame t Frame.(create
                            ~opcode: Opcode.Text
                            ~content: msg ());;

let handler sock ic oc =
    Reader.pipe ic |> fun rd ->
    Writer.pipe oc |> fun wr ->
    Pipe.transfer_id rd wr

let start ?cert_file ?key_file ~url ~handler () =
    let uri = Uri.of_string url in
    let port = Option.value_exn
        ~message: "no port inferred from scheme"
        Uri_services.(tcp_port_of_uri uri) in
    let host = Uri.host uri in

    let rec connection_loop addr_str sender_write receiver_read =
        Pipe.read receiver_read
        >>= function
            | `Eof ->
                info "Client %s disconnected" addr_str;
                Deferred.unit
            | `Ok ({ Frame.opcode; extension; final; content } as frame) ->
                let open Frame in
                debug "<- %s" Frame.(show frame);
                let frame', closed =
                    match opcode with
                    | Opcode.Ping -> Some Frame.(create ~opcode:Opcode.Pong ~content ()), false
                    | Opcode.Close -> None, true
                    | Opcode.Pong -> None, false
                    | Opcode.Text -> Some Frame.(create
                                        ~opcode: Opcode.Text
                                        ~content: (handler content) ()), false
                    | Opcode.Binary -> None, false
                    | _ -> Some Frame.(close 1002), false
                in
                match frame' with
                    | None ->
                        Deferred.unit
                    | Some frame' ->
                        debug "-> %s" Frame.(show frame');
                        Pipe.write sender_write frame'
                >>= fun () ->
                if closed then Deferred.unit else connection_loop addr_str sender_write receiver_read in

    let server_inst = {
        id = ref 0;
        sockets = Int.Table.create ()
    } in

    let tcp_callback addr reader writer =
        let addr_str = Socket.Address.(to_string addr) in
        debug "Client connection from %s, total: %d" addr_str (List.length (Int.Table.keys server_inst.sockets));
        let app_to_ws, sender_write = Pipe.create () in
        let receiver_read, ws_to_app = Pipe.create () in
        let inet_addr = match addr with `Inet _ as a -> a in
        let id = !(server_inst.id) in
        Int.Table.set server_inst.sockets
            ~key: id
            ~data: { sender_write; receiver_read };
        server_inst.id := !(server_inst.id) + 1;

        try_with (fun () ->
            let finished = Websocket_async.server
                ~log: Lazy.(force log)
                ~app_to_ws ~ws_to_app ~reader ~writer inet_addr
            in
            Deferred.any [connection_loop addr_str sender_write receiver_read; finished]
            >>| fun () ->
            Int.Table.remove server_inst.sockets id
        )
        >>| function
            | Ok () -> ()
            | Error msg -> debug "Error happened"
    in

    let determine_mode cert_file_path key_file_path =
        (* Determines if the server runs in http or https *)
        match (cert_file_path, key_file_path) with
            | Some c, Some k -> `OpenSSL (`Crt_file_path c, `Key_file_path k)
            | None, None -> `TCP
            | _ -> failwith "Error: must specify both certificate and key for TLS" in

    let start_server port host cert_file key_file () =
        let mode = determine_mode cert_file key_file in
        let mode_str = (match mode with `OpenSSL _ -> "OpenSSL" | `TCP -> "TCP") in
        printf "Listening for %s requests on: %s %d\n%!" mode_str host port;
        Unix.Inet_addr.of_string_or_getbyname host
        >>= fun host ->
        let listen_on = Tcp.Where_to_listen.create
            ~socket_type: Socket.Type.tcp
            ~address: (`Inet (host, port))
            ~listening_on: (fun _ -> port)
        in
        Conduit_async.serve
            ~on_handler_error: `Ignore
            mode
            listen_on tcp_callback
        >>= fun _ -> never () in

    (match host with
        | Some host ->
            ignore (start_server port host cert_file key_file ())
        | None -> failwith "Host should be specified");

    server_inst;;