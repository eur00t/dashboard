module M_p = Message_processor

module type Processor_client_inst = sig
    module Processor: M_p.Processor
    val update: Processor.Client.t -> unit
    val client: Processor.Client.t ref
end;;

module Client = struct
    type t = (string, (module Processor_client_inst)) Hashtbl.t

    let client_payload_to_string payload =
        Yojson.Safe.to_string
        (M_p.Client_payload.to_yojson payload)

    let send_client_payload ws payload =
        ws##send (Js.string (client_payload_to_string payload))

    let create l =
        let table: t = Hashtbl.create 10 in
        List.iter begin
            fun (module M: Processor_client_inst) ->
                Hashtbl.add table M.Processor.name (module M: Processor_client_inst)
        end l;
        table

    let initial_request t ws =
        Hashtbl.iter begin
            fun name _ ->
                send_client_payload ws (M_p.Client_payload.Full name);
        end t

    let process_server_payload t ws server_payload =
        match server_payload with
            | M_p.Server_payload.Full (name, version, data) -> begin
                try
                    let (module M: Processor_client_inst) = Hashtbl.find t name in
                    match M.Processor.set_client_state !M.client data version with
                        | Ok client -> Ok (M.update client)
                        | Error _ as err -> err
                with
                    Not_found -> Error ("Can't find processor " ^ name)
            end
            | M_p.Server_payload.Update (name, version, data) -> begin
                try
                    let (module M: Processor_client_inst) = Hashtbl.find t name in
                    match M.Processor.update_client_state !M.client data version with
                        | Ok client -> Ok (M.update client)
                        | Error "Version mismatch" ->
                            send_client_payload ws (M_p.Client_payload.Full name);
                            Ok ()
                        | Error _ as err -> err
                with
                    Not_found -> Error ("Can't find processor " ^ name)
            end
            | M_p.Server_payload.Empty -> Ok ()
end

let create_processor_client_inst
        (type a)
        (module P: M_p.Processor with type config = a)
        config =
    (module struct
        module Processor = P
        let client = ref (Processor.Client.create config)
        let update t =
            client := t;
            Processor.Client.print_state t;
    end: Processor_client_inst)

let client = Client.create [
    create_processor_client_inst (module Total_count_processor) { interval_s = 7200 }
]