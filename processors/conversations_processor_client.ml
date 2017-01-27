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

    let get_period_str start end_ =
        let start_time = Unix.localtime (float_of_int start) in
        let end_time = Unix.localtime (float_of_int end_) in
        let start_str = U.get_time_str start_time in
        let end_str = U.get_time_str end_time in
        if start_str = end_str then start_str
        else start_str ^ "—" ^ end_str

    let get_time class_name time_value =
        el `div ("time " ^ class_name) [
            el `div "dot" [];
            el `span "" [
                Text (U.get_time_str (Unix.localtime (float_of_int time_value)))
            ];
        ]

    let render_conv conv users_info =
        let users = conv.people
            |> U.table_to_pairs
            |> List.sort (fun (_, a) (_, b) -> b - a) in
        let l = List.length users in

        node_key
            `div
            ("conv" ^ (if conv.start = conv.end_ then " point" else " period"))
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
                        (Hashtbl.find users_info id))))
            ]

    let render ?title state =
        let str = Yojson.Safe.to_string (client_state_to_yojson state) in
        let obj = Js.Unsafe.global##.JSON##parse (Js.string str) in
        let pretty = Js.Unsafe.global##.JSON##stringify obj Js.null 4 in

        Firebug.console##log (Js.string (string_of_int (List.length state.convs)));

        node `div "processor-conversations"
            (begin
                match state.conv_current with
                    | Some conv -> [Elem (render_conv conv state.users_info)]
                    | None -> []
            end
            @
            begin
                List.map
                    (fun conv -> Elem (render_conv conv state.users_info))
                    state.convs
            end)
end

include Message_processor_client.Make_processor(Core)