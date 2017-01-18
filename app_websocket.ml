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

let start ~url ~handler =
    let uri = Uri.of_string url in
    let port = Option.value_exn
        ~message: "no port inferred from scheme"
        Uri_services.(tcp_port_of_uri uri) in

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

        let finished = Websocket_async.server
            ~log: Lazy.(force log)
            ~app_to_ws ~ws_to_app ~reader ~writer inet_addr
        in
        Deferred.any [connection_loop addr_str sender_write receiver_read; finished]
        >>| fun () ->
        Int.Table.remove server_inst.sockets id
    in

    ignore (Tcp.Server.create (Tcp.on_port port) tcp_callback);
    server_inst;;