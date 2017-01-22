module M_p = Message_processor

module type Processor_server_inst = sig
    module Processor: M_p.Processor
    val update: Processor.Server.t -> unit
    val server: Processor.Server.t ref
end;;

module Server = struct
    type t = (string, (module Processor_server_inst)) Hashtbl.t

    let server_payload_to_string payload =
        Yojson.Safe.to_string
        (M_p.Server_payload.to_yojson payload)

    let send_server_payload ws_server payload =
        App_websocket.send_all ws_server (server_payload_to_string payload)

    let create l =
        let table: t = Hashtbl.create 10 in
        List.iter begin
            fun (module M: Processor_server_inst) ->
                Hashtbl.add table M.Processor.name (module M: Processor_server_inst)
        end l;
        table

    let process_message ws_server t msg =
        Hashtbl.filter_map_inplace begin
            fun name (module M: Processor_server_inst) ->
                let (server, payload) = M.Processor.process_message !M.server msg in
                M.update server;
                ignore (send_server_payload ws_server payload);
                Some (module M)
        end t

    let process_client_payload t client_payload =
        match client_payload with
            | M_p.Client_payload.Full name
            | M_p.Client_payload.Update (name, _) -> begin
                try
                    let (module M: Processor_server_inst) = Hashtbl.find t name in
                    M.Processor.get_full_server_payload !M.server
                with
                    Not_found -> M_p.Server_payload.Empty
            end
end

let create_processor_server_inst
        (type a)
        (module P: M_p.Processor with type config = a)
        config =
    (module struct
        module Processor = P
        let server = ref (Processor.Server.create config)
        let update t = server := t
    end: Processor_server_inst)

let server = Server.create [
    create_processor_server_inst (module Total_count_processor) { interval_s = 7200 }
]