module Core = struct
    include Total_count_processor.Core

    let render ?title state =
        let open Reactjs in
        DOM.make ~tag: `div ~class_name: "processor-total-count" [
            Elem (DOM.make ~tag: `span [Text (string_of_int state)])
        ]
end

include Message_processor_client.Make_processor(Core)