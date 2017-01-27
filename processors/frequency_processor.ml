module Core = struct
    let default_name = "frequency"
    type server_state = (int * int) list [@@deriving yojson]
    type client_state = (int * int) list [@@deriving yojson]
    type config = { interval_s: int; decay_s: int } [@@deriving yojson]
    type update_atom =
        | Update of int * int
        | Insert of int * int
        | Remove of int [@@deriving yojson]
    type update = update_atom list [@@deriving yojson]

    let rec apply_update_atom ((t_u, c_u) as atom) l res =
        match l with
            | ((t, c) as el) :: rest ->
                if t = t_u then (t_u, c_u) :: rest, res
                else apply_update_atom atom rest (el :: res)
            | [] -> assert false

    let rec apply_remove_atom t_r l res =
        match l with
            | (t, c) :: rest ->
                if t = t_r then rest, res
                else assert false
            | [] -> assert false

    let rec apply_insert_atom ((t_i, c_i) as atom) l res =
        match l with
            | ((t, c) as el) :: rest ->
                if t > t_i then l, (t_i, c_i) :: res
                else apply_insert_atom atom rest (el :: res)
            | [] -> [], (t_i, c_i) :: res

    let apply_atom atom l res =
        match atom with
            | Update (t_u, c_u) -> apply_update_atom (t_u, c_u) l res
            | Remove t_r -> apply_remove_atom t_r l res
            | Insert (t_i, c_i) -> apply_insert_atom (t_i, c_i) l res

    let update_state l updates =
        let l, res = List.fold_left
            (fun (l, res) atom -> apply_atom atom l res)
            (l, []) updates in

        List.concat [(List.rev res); l]

    let rec process_list l t t_d decay_s =
        let c_opt, c_d_opt, actions =
            List.fold_left begin
                fun (c_opt, c_d_opt, actions) ((time, count) as p) ->
                    if (t - time) > decay_s then (c_opt, c_d_opt, (Remove time) :: actions)
                    else match c_opt, c_d_opt with
                        | Some c, Some c_d ->
                            if time < t then (Some (count + 1), Some count, actions)
                            else if time = t then (None, Some count, (Update (time, count + 1)) :: actions)
                            else if time > t_d then (None, None, (Insert (t_d, c_d)) :: (Insert (t, c)) :: actions)
                            else if time = t_d then (None, None, (Insert (t, c)) :: actions)
                            else (None, Some count, (Update (time, count + 1)) :: (Insert (t, c)) :: actions)

                        | None, Some c_d ->
                            if time < t_d then (None, Some count, (Update (time, count + 1)) :: actions)
                            else if time = t_d then (None, None, actions)
                            else (None, None, (Insert (t_d, c_d)) :: actions)

                        | None, None -> (None, None, actions)

                        | Some _, None -> assert false
            end (Some 1, Some 0, []) l in
        let actions = match c_opt with
            | Some c -> (Insert (t, c)) :: actions
            | None -> actions in
        let actions = match c_d_opt with
            | Some c_d -> (Insert (t_d, c_d)) :: actions
            | None -> actions in
        let actions = List.rev actions in
        (update_state l actions), Some actions

    let client_state_empty _ = []
    let server_state_empty _ = []
    let client_state_from_server_state l = l
    let update_client_state client_state updates = update_state client_state updates

    let process_message l { interval_s; decay_s } msg =
        let time = Chat_message.get_time msg in
        process_list l time (time + interval_s) decay_s
end

include Message_processor.Make_processor(Core)