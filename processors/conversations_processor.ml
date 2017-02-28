module U = Util_shared

module Hashtbl = U.Hashtbl_ext

let iterate_updates funcs server_state =
    let rec loop funcs server_state updates_accum =
        match funcs with
            | func :: rest ->
                let server_state, updates = func server_state in
                loop rest server_state (List.concat [updates_accum; updates])
            | [] -> (server_state, updates_accum) in
    loop funcs server_state []

let result_updates_to_option (server_state, updates) =
    match updates with
            | [] -> server_state, None
            | _ -> server_state, Some updates

module Core = struct
    let default_name = "conversations"
    type conv = {
        id: int;
        start: int;
        end_: int;
        people: (string, int) Hashtbl.t; (* id, msgs_count *)
    } [@@deriving yojson]
    type user_info = {
        first_name: string;
        last_name: string;
        photo: string;
    } [@@deriving yojson]
    type server_state = {
        next_conv_id: int;
        convs: conv Queue.t;
        conv_current: conv option;
        users_info: (string, user_info) Hashtbl.t;
        user_refs: (string, int) Hashtbl.t (* id, convs_count *)
    }
    type client_state = {
        convs: conv list;
        conv_current: conv option;
        users_info: (string, user_info) Hashtbl.t;
    } [@@deriving yojson]
    type config = { interval_s: int; decay_s: int } [@@deriving yojson]
    type update_atom =
        | Push_current
        | New_current of conv
        | Update_current of int * (string * int) (* end_, (from, count) *)
        | Add_user_info of string * user_info
        | Update_user_info of string * user_info
        | Remove_conv of int
        | Remove_user_info of string
        [@@deriving yojson]
    type update = update_atom list [@@deriving yojson]

    let next_id = ref 0

    let get_next_id server_state = server_state.next_conv_id

    let client_state_empty _ = {
        convs = [];
        conv_current = None;
        users_info = Hashtbl.create 10
    }

    let server_state_empty _ = {
        next_conv_id = 0;
        convs = Queue.create ();
        conv_current = None;
        users_info = Hashtbl.create 10;
        user_refs = Hashtbl.create 10
    }

    let client_state_from_server_state (server_state: server_state) = {
        convs = U.queue_to_list server_state.convs;
        conv_current = server_state.conv_current;
        users_info = server_state.users_info
    }

    let update_client_state_single client_state update =
        match update with
            | Update_current (end_, (from, count)) ->
                let conv_current =
                match client_state.conv_current with
                    | Some conv ->
                        Hashtbl.replace conv.people from count;
                        Some { conv with end_ }
                    | None -> None in
                {
                    client_state with conv_current
                }
            | Push_current ->
                (match client_state.conv_current with
                    | Some conv ->
                        {
                            client_state with
                            convs = conv :: client_state.convs
                        }
                    | None -> client_state)
            | New_current conv ->
                {
                    client_state with
                    conv_current = Some conv
                }
            | Add_user_info (from, user_info)
            | Update_user_info (from, user_info) ->
                Hashtbl.replace client_state.users_info from user_info;
                client_state
            | Remove_conv id ->
                let convs = List.filter (fun conv -> conv.id != id) client_state.convs in
                { client_state with convs }
            | Remove_user_info from ->
                Hashtbl.remove client_state.users_info from;
                client_state

    let update_client_state client_state updates =
        List.fold_left update_client_state_single client_state updates

    let add_user_ref from server_state =
        ignore ((U.table_inc server_state.user_refs from): bool);
        server_state, []

    let process_user_info from user_info (server_state: server_state) =
        match U.table_get server_state.users_info from with
            | Some saved_user_info ->
                if not (saved_user_info = user_info)
                then begin
                    Hashtbl.replace server_state.users_info from user_info;
                    server_state, [Update_user_info (from, user_info)]
                end
                else server_state, []
            | None ->
                Hashtbl.add server_state.users_info from user_info;
                server_state, [Add_user_info (from, user_info)]

    let update_current_iter conv time from server_state =
        let server_state, updates =
            if not (U.table_inc conv.people from) then
                add_user_ref from server_state
            else
                server_state, [] in
        let server_state = {
            server_state with
            conv_current = Some {
                conv with
                end_ = time
            }
        } in
        server_state,
        (Update_current (time, (from, Hashtbl.find conv.people from))) :: updates

    let create_conv time from (server_state: server_state) =
        let people = Hashtbl.create 10 in
        Hashtbl.add people from 1;
        let conv = {
            id = get_next_id server_state;
            start = time;
            end_ = time;
            people
        } in
        {
            server_state with
            next_conv_id = server_state.next_conv_id + 1;
            conv_current = Some conv
        }, [New_current conv]

    let push_current (server_state: server_state) =
        match server_state.conv_current with
            | Some conv ->
                Queue.add conv server_state.convs;
                server_state, [Push_current]
            | None -> server_state, []

    let clean_conv conv server_state =
        let updates_opt = List.map begin
            fun key ->
                if not (U.table_dec server_state.user_refs key) then begin
                    Hashtbl.remove server_state.users_info key;
                    Some (Remove_user_info key)
                end else
                    None
        end (Hashtbl.keys conv.people) in
        let updates = U.filter_some updates_opt in
        server_state, (Remove_conv conv.id) :: updates

    let clean_convs decay_s time (server_state: server_state) =
        let q = server_state.convs in
        let remove = ref [] in
        while (
            (not (Queue.is_empty q)) &&
            (time - (Queue.peek q).end_) > decay_s
        ) do
            let conv = Queue.pop q in
            remove := conv :: !remove
        done;
        iterate_updates
            (List.map (fun conv -> clean_conv conv) !remove)
            server_state

    let add_new_conv (server_state: server_state) time from user_info decay_s =
        iterate_updates [
            push_current;
            create_conv time from;
            add_user_ref from;
            process_user_info from user_info;
            clean_convs decay_s time
        ] server_state
        |> result_updates_to_option

    let update_current server_state conv time from user_info decay_s =
        iterate_updates [
            update_current_iter conv time from;
            process_user_info from user_info;
            clean_convs decay_s time
        ] server_state
        |> result_updates_to_option

    let process_message_user_info (server_state: server_state) interval_s decay_s time from user_info =
        match server_state.conv_current with
            | Some conv ->
                if (time - conv.end_) < interval_s
                then update_current server_state conv time from user_info decay_s
                else add_new_conv server_state time from user_info decay_s
            | None ->
                add_new_conv server_state time from user_info decay_s

    let process_message (server_state: server_state) { interval_s; decay_s } msg =
        let module C = Chat_message in
        let time = C.get_time msg in
        match U.deoption_tuple4 (
            C.get_from msg,
            C.get_first_name msg,
            C.get_last_name msg,
            C.get_photo msg
        ) with
            | Some (from, first_name, last_name, photo) ->
                process_message_user_info server_state interval_s decay_s time from {
                    first_name; last_name; photo
                }
            | None -> server_state, None
end

include Message_processor.Make_processor(Core)