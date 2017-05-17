open Reactjs
open Reactjs.Infix
open Util_react

module U = Util_shared

module Core = struct
    include Last_seen_processor.Core

    let render ?title { users_info; last_message_pos } ~config =
        let open Reactjs in
        let open Util_react in
        let pairs =
            Util_shared.table_to_pairs users_info
            |> List.sort begin
                fun (_, a) (_, b) -> a.message_pos - b.message_pos
            end
        in
        let users_dom =
            List.map begin
                fun (id, info) ->
                    let local_timestamp = (Unix.localtime (float_of_int info.timestamp)) in
                    Elem (DOM.make
                        ~elem_spec: (object%js
                            val key = !* id
                        end)
                        ~tag: `li
                        ~class_name: "user"

                        [
                            el `div "photo" [
                                Elem (DOM.make
                                    ~elem_spec: (object%js
                                        val src = !* (info.photo)
                                        val title = !* (info.first_name ^ " " ^ info.last_name)
                                    end)
                                    ~tag: `img
                                    [])
                            ];
                            el `div "messages" [
                                Text (string_of_int (last_message_pos - info.message_pos))
                            ];
                            el `div "timestamp" [
                                Text (
                                    (U.get_date_str local_timestamp) ^
                                    " " ^
                                    (U.get_time_str local_timestamp)
                                )
                            ]
                        ])
            end pairs
        in
        DOM.make ~tag: `ul ~class_name: "processor-last-seen" users_dom
end

include Message_processor_client.Make_processor(Core)