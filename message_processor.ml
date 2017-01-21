module Client_payload = struct
    type t =
        | Full of string (* name *)
        | Update of string * int (* name, version *)
    [@@deriving yojson]
end

module Server_payload = struct
    type t =
        | Full of string * int * Yojson.Safe.json (* name, version, data *)
        | Update of string * int * Yojson.Safe.json (* name, version, data *)
        | Empty
    [@@deriving yojson]
end;;

module type Processor_core = sig
    val name: string
    type server_state
    type client_state [@@deriving yojson]
    type config [@@deriving yojson]
    type update [@@deriving yojson]

    val client_state_empty: config -> client_state
    val server_state_empty: config -> server_state
    val client_state_from_server_state: server_state -> client_state
    val update_client_state: client_state -> update -> client_state
    val process_message: server_state -> config -> Chat_message.t -> server_state * update
end

module type Processor = sig
    type config
    val name: string

    module Server: sig
        type t
        val create: config -> t
    end

    module Client: sig
        type t
        val print_state: t -> unit
        val create: config -> t
    end

    val set_client_state: Client.t -> Yojson.Safe.json -> int -> (Client.t, string) result
    val get_full_server_payload: Server.t -> Server_payload.t
    val update_client_state: Client.t -> Yojson.Safe.json -> int -> (Client.t, string) result
    val process_message: Server.t -> Chat_message.t -> Server.t * Server_payload.t
end

module Make_processor (Core: Processor_core) = struct
    type config = Core.config
    let name = Core.name

    module Server = struct
        type t = {
            c: config;
            state: Core.server_state;
            version: int
        }

        let create config = {
            c = config;
            state = Core.server_state_empty config;
            version = 0
        }
    end

    module Client = struct
        type t = {
            c: config;
            state: Core.client_state;
            version: int
        }

        let print_state t =
            print_string (Yojson.Safe.to_string (Core.client_state_to_yojson t.state));
            flush_all ()

        let create config = {
            c = config;
            state = Core.client_state_empty config;
            version = 0
        }
    end

    let set_client_state t json version =
        match Core.client_state_of_yojson json with
            | Ok state -> Ok {
                t with
                Client.state; version
            }
            | Error _ as err -> err

    let get_full_server_payload t =
        let open Server_payload in
        Full (
            Core.name,
            t.Server.version,
            Core.client_state_to_yojson (Core.client_state_from_server_state t.Server.state)
        )

    let update_client_state t json version =
        if t.Client.version <> version - 1 then Error "Version mismatch"
        else match Core.update_of_yojson json with
            | Ok update -> Ok {
                t with
                Client.state = Core.update_client_state t.Client.state update;
                version
            }
            | Error _ as err -> err

    let process_message t msg =
        let (state, update) = Core.process_message t.Server.state t.Server.c msg in
        let version = t.Server.version + 1 in
        let open Server_payload in
        {
            t with
            Server.state; version
        },
        Update (
            Core.name,
            version,
            Core.update_to_yojson update
        )
end

module type Processor_server_inst = sig
    module Processor: Processor
    val update: Processor.Server.t -> unit
    val server: Processor.Server.t ref
end;;

module type Processor_client_inst = sig
    module Processor: Processor
    val update: Processor.Client.t -> unit
    val client: Processor.Client.t ref
end;;

let create_processor_server_inst
        (type a)
        (module P: Processor with type config = a)
        config =
    (module struct
        module Processor = P
        let server = ref (Processor.Server.create config)
        let update t = server := t
    end: Processor_server_inst)

let create_processor_client_inst
        (type a)
        (module P: Processor with type config = a)
        config =
    (module struct
        module Processor = P
        let client = ref (Processor.Client.create config)
        let update t =
            client := t;
            Processor.Client.print_state t;
    end: Processor_client_inst)

module Total_count_processor_core = struct
    let name = "total"
    type server_state = int Queue.t
    type client_state = int [@@deriving yojson]
    type config = { interval_s: int } [@@deriving yojson]
    type update = int [@@deriving yojson]

    let client_state_empty _ = 0
    let server_state_empty _ = Queue.create ()
    let client_state_from_server_state q = Queue.length q
    let update_client_state count delta = count + delta
    let process_message q { interval_s } msg =
        let time = Chat_message.get_time msg in
        Queue.add time q;
        let delta = begin
            let delta_ref = ref 1 in
            while (
                (not (Queue.is_empty q)) &&
                (time - (Queue.peek q)) > interval_s
            ) do
                ignore (Queue.take q);
                delta_ref := !delta_ref - 1;
            done;
            !delta_ref
        end in
        q, delta
end

module Total_count_processor = Make_processor(Total_count_processor_core)