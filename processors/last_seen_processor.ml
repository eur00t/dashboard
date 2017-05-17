module U = Util_shared

module Hashtbl = U.Hashtbl_ext

module Core = struct
    let default_name = "last_seen"
    type user_info = {
        timestamp: int;
        message_pos: int;
        photo: string;
        first_name: string;
        last_name: string;
    } [@@deriving yojson]
    type server_state = {
        users_info: (string, user_info) Hashtbl.t;
        last_message_pos: int
    } [@@deriving yojson]
    type client_state = server_state [@@deriving yojson]
    type config = () [@@deriving yojson]
    type update = {
        user_id: string;
        user_info: user_info;
        next_last_message_pos: int
    } [@@deriving yojson]

    let server_state_empty _ = {
        users_info = Hashtbl.create 50;
        last_message_pos = 0
    }
    let client_state_empty _ = server_state_empty ()

    let client_state_from_server_state q = q
    let update_client_state client_state { user_id; user_info; next_last_message_pos } =
        Hashtbl.replace client_state.users_info user_id user_info;
        {
            client_state with
            last_message_pos = next_last_message_pos
        }

    let process_message server_state _ msg =
        let module C = Chat_message in
        let time = C.get_time msg in
        let last_message_pos = server_state.last_message_pos + 1 in
        match U.deoption_tuple4 (
            C.get_from msg,
            C.get_first_name msg,
            C.get_last_name msg,
            C.get_photo msg
        ) with
            | Some (from, first_name, last_name, photo) ->
                let user_info = {
                    timestamp = time;
                    message_pos = last_message_pos;
                    photo; first_name; last_name
                } in
                Hashtbl.replace server_state.users_info from user_info;

                { server_state with last_message_pos },
                Some { user_id = from; user_info; next_last_message_pos = last_message_pos }
            | None -> server_state, None
end

include Message_processor.Make_processor(Core)