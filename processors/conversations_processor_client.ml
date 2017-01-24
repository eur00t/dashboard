module Core = struct
    include Conversations_processor.Core

    let render ?title state =
        let str = Yojson.Safe.to_string (client_state_to_yojson state) in
        let obj = Js.Unsafe.global##.JSON##parse (Js.string str) in
        let pretty = Js.Unsafe.global##.JSON##stringify obj Js.null 4 in

        let open Reactjs in
        DOM.make ~tag: `div ~class_name: "processor-conversations" [
            Elem (DOM.make ~tag: `pre [
                Text (Js.to_string pretty)
            ])
        ]
end

include Message_processor_client.Make_processor(Core)