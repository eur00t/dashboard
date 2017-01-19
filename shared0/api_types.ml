type x = {
    str: string
} [@@deriving yojson];;

module Initial_request = struct
    type t = int [@@deriving yojson]
end;;

module Initial_response = struct
    type t = { test: int } [@@deriving yojson]
end;;

type msg_from_client =
    | Initial_request of Initial_request.t
    [@@deriving yojson]
;;

type msg_from_server =
    | Initial_response of Initial_response.t
    [@@deriving yojson]
;;

let server_handler ~handler =
    fun raw_str ->
        let msg = match msg_from_client_of_yojson (Yojson.Safe.from_string raw_str) with
            | Ok msg -> msg
            | Error msg -> failwith msg in
        let msg_out = handler msg in
        Yojson.Safe.to_string (msg_from_server_to_yojson msg_out);;

let client_handler raw_str ~handler =
    match msg_from_server_of_yojson (Yojson.Safe.from_string raw_str) with
        | Ok msg -> handler msg
        | Error msg -> failwith msg;;

let raw_msg_from_client msg = Yojson.Safe.to_string (msg_from_client_to_yojson msg);;

let func x = x + 2;;