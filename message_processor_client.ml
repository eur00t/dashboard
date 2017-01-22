module M_p = Message_processor

module type Processor_core = sig
    include M_p.Processor_core

    val render: client_state -> Reactjs.Low_level_bindings.react_element Js.t
end

module type Processor = sig
    include M_p.Processor

    val get_react_class: Client.t Bus.t -> Reactjs.Low_level_bindings.react_class Js.t
end

module Make_processor (Core: Processor_core) = struct
    include M_p.Make_processor(Core)

    let get_react_class bus =
        let open Reactjs in
        make_class_spec
            ~initial_state: begin
                fun ~this ->
                    let client = Bus.get_last bus in
                    object%js
                        val state_str = Yojson.Safe.to_string (Core.client_state_to_yojson client.Client.state)
                    end
            end
            ~component_did_mount: begin
                fun ~this ->
                    this##.bus_id := Bus.on bus (
                        fun client ->
                            this##setState (
                                object%js
                                    val state_str = Yojson.Safe.to_string (Core.client_state_to_yojson client.Client.state)
                                end
                            )
                    )
            end
            ~component_will_unmount: begin
                fun ~this ->
                    Bus.off bus this##.bus_id
            end

            begin
                fun ~this ->
                    match Core.client_state_of_yojson (Yojson.Safe.from_string this##.state##.state_str) with
                        | Ok state -> Core.render state
                        | Error msg -> DOM.make ~tag:`p [Text ("Error happened: " ^ msg)]

            end
        |> create_class
end