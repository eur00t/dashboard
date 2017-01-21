module M_p = Message_processor

module Server = struct
    type t = (string, (module M_p.Processor_server_inst)) Hashtbl.t

    let server_payload_to_string payload =
        Yojson.Safe.to_string
        (M_p.Server_payload.to_yojson payload)

    let send_server_payload ws_server payload =
        App_websocket.send_all ws_server (server_payload_to_string payload)

    let create l =
        let table: t = Hashtbl.create 10 in
        List.iter begin
            fun (module M: M_p.Processor_server_inst) ->
                Hashtbl.add table M.Processor.name (module M: M_p.Processor_server_inst)
        end l;
        table

    let process_message ws_server t msg =
        Hashtbl.filter_map_inplace begin
            fun name (module M: M_p.Processor_server_inst) ->
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
                    let (module M: M_p.Processor_server_inst) = Hashtbl.find t name in
                    M.Processor.get_full_server_payload !M.server
                with
                    Not_found -> M_p.Server_payload.Empty
            end
end

let server = Server.create [
    M_p.create_processor_server_inst (module M_p.Total_count_processor) { interval_s = 5 }
]