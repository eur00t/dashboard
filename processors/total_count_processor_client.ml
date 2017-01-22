module Core = struct
    include Total_count_processor.Core

    let render state =
        let open Reactjs in
        DOM.make ~tag: `span [Text (string_of_int state)]
end

include Message_processor_client.Make_processor(Core)