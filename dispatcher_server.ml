module M_p = Message_processor
module U = Util_shared

module type Processor_server_inst = sig
    module Processor: M_p.Processor
    val update: Processor.Server.t -> unit
    val server: Processor.Server.t ref
end;;

module Server = struct
    type t = (string, (module Processor_server_inst)) Hashtbl.t

    let dump_to_file server filename =
        let marsh_dict = server
            |> U.table_to_pairs
            |> List.map (fun (name, (module M: Processor_server_inst)) ->
                (name, Marshal.to_string (!M.server) [])) in
        let marsh_str = Marshal.to_string marsh_dict [] in
        Core.Std.Out_channel.write_all filename ~data: marsh_str

    let read_dump_from_file server filename =
        let marsh_str = Core.Std.In_channel.read_all filename in
        let marsh_dict = Marshal.from_string marsh_str 0 in
        List.iter begin
            fun (name, server_marsh_str) ->
                if Hashtbl.mem server name then
                let (module M: Processor_server_inst) = Hashtbl.find server name in
                M.update (Marshal.from_string server_marsh_str 0)
        end marsh_dict

    let server_payload_to_string payload =
        Yojson.Safe.to_string
        (M_p.Server_payload.to_yojson payload)

    let send_server_payload ws_server payload =
        App_websocket.send_all ws_server (server_payload_to_string payload)

    let create l =
        let table: t = Hashtbl.create 10 in
        List.iter begin
            fun (module M: Processor_server_inst) ->
                Hashtbl.add table (M.Processor.Server.get_name !(M.server))  (module M: Processor_server_inst)
        end l;
        table

    let process_message ws_server t msg =
        Hashtbl.filter_map_inplace begin
            fun name (module M: Processor_server_inst) ->
                let (server, maybe_payload) = M.Processor.process_message !M.server msg in
                M.update server;
                (match maybe_payload with
                    | None -> ()
                    | Some payload -> ignore (send_server_payload ws_server payload));
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
        ?name
        (module P: M_p.Processor with type config = a)
        config =
    (module struct
        module Processor = P
        let server = ref (Processor.Server.create ?name config)
        let update t = server := t
    end: Processor_server_inst)

let server = Server.create [
    (*create_processor_server_inst (module Total_count_processor) { interval_s = 7200 };*)
    create_processor_server_inst (module Total_count_processor) { interval_s = 60 } ~name: "total_minute";
    create_processor_server_inst (module Conversations_processor) { interval_s = 60 * 10; decay_s = 60 * 60 * 24 }
]