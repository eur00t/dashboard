module M_p = Message_processor

module type Processor_core = sig
    include M_p.Processor_core

    val render: client_state -> Reactjs.Low_level_bindings.react_element Js.t
end

module type Processor = sig
    include M_p.Processor

    module Client: sig
        type t
        val print_state: t -> unit
        val create: ?name: string -> config -> t
        val get_name: t -> string
    end

    val set_client_state: Client.t -> Yojson.Safe.json -> int -> (Client.t, string) result
    val update_client_state: Client.t -> Yojson.Safe.json -> int -> (Client.t, string) result
    val get_react_class: Client.t Bus.t -> Reactjs.Low_level_bindings.react_class Js.t
end

module Make_processor (Core: Processor_core) = struct
    include M_p.Make_processor(Core)

    module Client = struct
        type t = {
            c: config;
            name: string;
            state: Core.client_state;
            version: int
        }

        let print_state t =
            print_string (Yojson.Safe.to_string (Core.client_state_to_yojson t.state));
            flush_all ()

        let create ?name config = {
            c = config;
            name = (match name with
                | Some str -> str
                | None -> Core.default_name);
            state = Core.client_state_empty config;
            version = 0
        }

        let get_name { name } = name
    end

    let set_client_state t json version =
        match Core.client_state_of_yojson json with
            | Ok state -> Ok {
                t with
                Client.state; version
            }
            | Error _ as err -> err

    let update_client_state t json version =
        if t.Client.version <> version - 1 then Error "Version mismatch"
        else match Core.update_of_yojson json with
            | Ok update -> Ok {
                t with
                Client.state = Core.update_client_state t.Client.state update;
                version
            }
            | Error _ as err -> err

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