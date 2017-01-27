open Util_react

module Core = struct
    include Frequency_processor.Core

    let render ?title client_state =
        let json_str = Yojson.Safe.to_string (client_state_to_yojson client_state) in
        let json = Js.Unsafe.global##.JSON##parse json_str in
        let pretty = Js.Unsafe.global##.JSON##stringify json Js.null (Js.number_of_float 4.) in

        let open Reactjs in
        node `div "processor-frequency" [
            el `pre "json" [
                Text (Js.to_string pretty)
            ]
        ]
end

include Message_processor_client.Make_processor(Core)