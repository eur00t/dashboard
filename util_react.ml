open Reactjs
open Reactjs.Infix

let node_key tag class_name key children =
    DOM.make
        ~elem_spec: (object%js val key = !* key end)
        ~tag: tag
        ~class_name: class_name children

let el_key tag class_name key children = Elem (node_key tag class_name key children)

let node tag class_name children =
    DOM.make
        ~tag: tag
        ~class_name: class_name children

let el tag class_name children = Elem (node tag class_name children)

let component_bus ?bus ?initial_state render_func =
    let bus = match bus, initial_state with
        | Some bus_, None -> bus_
        | None, Some initial_state_ -> Bus.create initial_state_
        | Some _, Some _
        | None, None -> failwith "component_bus: wrong parameters" in
    let open Reactjs in
    let component_class = make_class_spec
        ~initial_state: begin
            fun ~this ->
                let state = Bus.get_last bus in
                object%js
                    val state_value = Json.output state
                end
        end
        ~component_did_mount: begin
            fun ~this ->
                this##.bus_id := Bus.on bus (
                    fun state ->
                        this##setState (
                            object%js
                                val state_value = Json.output state
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
                render_func (Json.unsafe_input this##.state##.state_value)
        end
    |> create_class in
    component_class, bus
