open Reactjs
open Reactjs.Infix
open Util_react

module U = Util_shared

module Core = struct
    include Conversations_processor.Core
    type user_badge =
        | Gold
        | Silver
        | Normal

    let badge_to_string b =
        match b with
            | Gold -> "gold"
            | Silver -> "silver"
            | Normal -> "normal"

    let render_user badge i id num user_info =
        DOM.make
            ~elem_spec: (object%js
                val key = !* id
                val style = object%js
                    val zIndex = !* (string_of_int i)
                end
            end)
            ~tag: `li
            ~class_name: ("user" ^ " " ^ (badge_to_string badge))
            [
                el `div "photo" [
                    Elem (DOM.make
                        ~elem_spec: (object%js val src = !* (user_info.photo) end)
                        ~tag: `img
                        [])
                ];
                el `div ("count" ^ (if num = 1 then " single" else ""))  [
                    Text (string_of_int num)
                ]
            ]

    let get_time class_name time_value =
        el `div ("time " ^ class_name) [
            el `div "dot" [];
            el `span "" [
                Text (U.get_time_str (Unix.localtime (float_of_int time_value)))
            ];
        ]

    let if_new_date prev_conv_opt conv =
        match prev_conv_opt with
            | Some prev_conv ->
                let open Unix in
                let prev_time = localtime (float_of_int prev_conv.start) in
                let cur_time = localtime (float_of_int conv.start) in
                prev_time.tm_mday != cur_time.tm_mday ||
                prev_time.tm_mon != cur_time.tm_mon ||
                prev_time.tm_year != cur_time.tm_year
            | None -> true

    let render_conv prev_conv_opt conv users_info =
        let users = conv.people
            |> U.table_to_pairs
            |> List.sort (fun (_, a) (_, b) -> b - a) in
        let l = List.length users in

        node_key
            `div
            ("conv"
                ^ (if conv.start = conv.end_ then " point" else " period")
                ^ (if (if_new_date prev_conv_opt conv) then " new-date" else ""))
            (string_of_int conv.id)
            [
                el `div "line" [];
                get_time "end" conv.end_;
                get_time "start" conv.start;
                el `ul "users"
                    (users
                    |> List.mapi (fun i (id, num) -> Elem (render_user
                        (match i with
                            | 0 -> Gold
                            | 1 -> Silver
                            | _ -> Normal)
                        (l - i - 1)
                        id num
                        (Hashtbl.find users_info id))));
                el `div "date" [
                    Text ((U.get_date_str (Unix.localtime (float_of_int conv.start))) ^ "↑")
                ]
            ]

    let render ?title state =
        let str = Yojson.Safe.to_string (client_state_to_yojson state) in
        let obj = Js.Unsafe.global##.JSON##parse (Js.string str) in
        let pretty = Js.Unsafe.global##.JSON##stringify obj Js.null 4 in

        Firebug.console##log (Js.string (string_of_int (List.length state.convs)));

        let convs = match state.conv_current with
            | Some conv -> conv :: state.convs
            | None -> state.convs in

        node `div "processor-conversations"
            (
                let _, res =
                (List.fold_left
                    (fun (prev_conv_opt, res) conv ->
                        Some conv, (Elem (render_conv prev_conv_opt conv state.users_info)) :: res)
                    (None, [])
                    (List.rev convs)) in res
            )
end

include Message_processor_client.Make_processor(Core)