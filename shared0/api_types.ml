module M_p = Message_processor

let server_handler ~handler =
    fun raw_str ->
        let msg = match M_p.Client_payload.of_yojson (Yojson.Safe.from_string raw_str) with
            | Ok msg -> msg
            | Error msg -> failwith msg in
        let msg_out = handler msg in
        Yojson.Safe.to_string (M_p.Server_payload.to_yojson msg_out);;

let client_handler raw_str ~handler =
    match M_p.Server_payload.of_yojson (Yojson.Safe.from_string raw_str) with
        | Ok msg -> handler msg
        | Error msg -> failwith msg;;