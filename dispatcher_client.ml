module M_p = Message_processor
module M_p_c = Message_processor_client

module type Processor_client_inst = sig
    module Processor: M_p_c.Processor
    val update: Processor.Client.t -> unit
    val update_bus: Processor.Client.t Bus.t
    val client: Processor.Client.t ref
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
        let title = M.Processor.Client.get_title !M.client in
        Elem (DOM.make
            ~tag: `div
            ~class_name: "processor"
            ~elem_spec: (object%js
                val key = !* name
            end)
            ((match title with
                | Some str -> [
                    Elem (DOM.make
                        ~tag: `div
                        ~class_name: "title"
                        [
                            Text str
                        ]
                    )
                ]
                | None -> []) @
            [
                Elem (DOM.make
                    ~tag: `div
                    ~class_name: "content"
                    [
                        Elem (create_element_from_class M.react_class)
                    ]
                )
            ])
        )

    let render t container_id =
        let open Reactjs in
        let react_elem =
            make_class_spec
            begin
                fun ~this ->
                    DOM.make
                        ~tag: `div
                        ~class_name: "processors"
                        [
                            Elem (DOM.make
                                ~tag: `div
                                ~class_name: "processors-inner"
                                (Hashtbl.fold (fun name inst res -> (render_processor name inst) :: res) t [])
                            )
                        ]
            end
            |> create_class
            |> create_element_from_class in
        render ~react_elem (get_elem ~id: container_id)
end

let create_processor_client_inst
        (type a)
        ?name
        ?title
        (module P: M_p_c.Processor with type config = a)
        config =
    (module struct
        module Processor = P
        let client = ref (Processor.Client.create ?name ?title config)
        let update_bus = Bus.create !client
        let update t =
            client := t;
            Bus.emit update_bus t;
            (*Processor.Client.print_state t*)
            ()
        let react_class = Processor.get_react_class !client update_bus
    end: Processor_client_inst)

let client = Client.create [
    (*create_processor_client_inst (module Total_count_processor_client)
        { interval_s = 7200 };*)
    create_processor_client_inst (module Total_count_processor_client)
        { interval_s = 60 }
        ~name: "total_minute"
        ~title: "Number of messages since 1 minute from last one";
    create_processor_client_inst (module Conversations_processor_client) { interval_s = 10; decay_s = 10 }
]