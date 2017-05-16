module Core = struct
    include Last_seen_processor.Core

    let render ?title { users_info; last_message_pos } ~config =
        let open Reactjs in
        let open Util_react in
        let pairs = Util_shared.table_to_pairs users_info in
        let users_dom =
        List.map begin
            fun (id, info) ->
                el `div "user" [
                    el `div "id" [ Text id];
                    el `div "timestamp" [ Text (string_of_int info.timestamp)]
                ]
        end pairs in

        DOM.make ~tag: `div ~class_name: "processor-last-seen" users_dom
end

include Message_processor_client.Make_processor(Core)