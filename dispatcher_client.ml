module M_p = Message_processor
module M_p_c = Message_processor_client

module type Processor_client_inst = sig
    module Processor: M_p_c.Processor
    val update: Processor.Client.t -> unit
    val update_bus: Processor.Client.t Bus.t
    val client: Processor.Client.t ref
    val order: int
    val react_class: Reactjs.Low_level_bindings.react_class Js.t
end;;

module Client = struct
    type t = (string, (module Processor_client_inst)) Hashtbl.t

    let client_payload_to_string payload =
        Yojson.Safe.to_string
        (M_p.Client_payload.to_yojson payload)

    let send_client_payload ws payload =
        ws##send (Js.string (client_payload_to_string payload))

    let create l =
        let table: t = Hashtbl.create 10 in
        List.iter begin
            fun (module M: Processor_client_inst) ->
                Hashtbl.add table (M.Processor.Client.get_name !(M.client)) (module M: Processor_client_inst)
        end l;
        table

    let initial_request t ws =
        Hashtbl.iter begin
            fun name _ ->
                send_client_payload ws (M_p.Client_payload.Full name);
        end t

    let process_server_payload t ws server_payload =
        match server_payload with
            | M_p.Server_payload.Full (name, version, data) -> begin
                try
                    let (module M: Processor_client_inst) = Hashtbl.find t name in
                    match M.Processor.set_client_state !M.client data version with
                        | Ok client -> Ok (M.update client)
                        | Error _ as err -> err
                with
                    Not_found -> Error ("Can't find processor " ^ name)
            end
            | M_p.Server_payload.Update (name, version, data) -> begin
                try
                    let (module M: Processor_client_inst) = Hashtbl.find t name in
                    match M.Processor.update_client_state !M.client data version with
                        | Ok client -> Ok (M.update client)
                        | Error "Version mismatch" ->
                            send_client_payload ws (M_p.Client_payload.Full name);
                            Ok ()
                        | Error _ as err -> err
                with
                    Not_found -> Error ("Can't find processor " ^ name)
            end
            | M_p.Server_payload.Empty -> Ok ()

    let render_processor name (module M: Processor_client_inst) =
        let open Reactjs in
        let open Reactjs.Infix in
        let open Util_react in
        let title = M.Processor.Client.get_title !M.client in
        Elem (DOM.make
            ~tag: `div
            ~class_name: "processor"
            ~elem_spec: (object%js
                val key = !* name
            end)
            ((match title with
                | Some str -> [
                    el `div "title" [ Text str ]
                ]
                | None -> []) @
            [
                el `div "content" [
                    Elem (create_element_from_class M.react_class)
                ]
            ])
        )

    let render t container_id child_components =
        let open Reactjs in
        let open Util_react in
        let processors = List.sort
            (fun (_, (module M1: Processor_client_inst)) (_, (module M2: Processor_client_inst)) -> M2.order - M1.order)
            (Hashtbl.fold (fun name inst res -> (name, inst) :: res) t []) in
        let react_elem =
            begin
                fun ~this ->
                    node `div "main"
                    ((List.map (fun comp -> Elem (create_element_from_class comp)) child_components)
                    @
                    [
                        el `div "processors" [
                            el `div "processors-inner"
                                (List.fold_left (fun res (name, inst) -> (render_processor name inst) :: res) [] processors)
                        ]
                    ])
            end
            |> make_class_spec
            |> create_class
            |> create_element_from_class in
        render ~react_elem (get_elem ~id: container_id)
end

let create_processor_client_inst
        (type a)
        ?name
        ?title
        (module P: M_p_c.Processor with type config = a)
        order
        config =
    (module struct
        module Processor = P
        let client = ref (Processor.Client.create ?name ?title config order)
        let order = order
        let update_bus = Bus.create !client
        let update t =
            client := t;
            Bus.emit update_bus t;
            (*Processor.Client.print_state t*)
            ()
        let react_class = Processor.get_react_class !client update_bus
    end: Processor_client_inst)

let client = Client.create [
    create_processor_client_inst (module Total_count_processor_client)
        3
        { interval_s = 60 * 60 }
        ~name: "total_hour"
        ~title: "1 hour";

    create_processor_client_inst (module Total_count_processor_client)
        2
        { interval_s = 60 }
        ~name: "total_minute"
        ~title: "1 minute before the last one";

    create_processor_client_inst (module Frequency_processor_client)
        1
        { interval_s = 60 * 10; decay_s = 60 * 60 * 24 }
        ~title: "10min intervals";

    create_processor_client_inst (module Conversations_processor_client)
        0
        { interval_s = 60 * 10; decay_s = 60 * 60 * 24 }
]