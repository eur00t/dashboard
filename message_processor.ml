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
    val default_name: string
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

    module Server: sig
        type t
        val create: ?name: string -> config -> t
        val get_name: t -> string
    end

    val get_full_server_payload: Server.t -> Server_payload.t
    val process_message: Server.t -> Chat_message.t -> Server.t * Server_payload.t
end

module Make_processor (Core: Processor_core) = struct
    type config = Core.config

    module Server = struct
        type t = {
            c: config;
            name: string;
            state: Core.server_state;
            version: int
        }

        let create ?name config = {
            c = config;
            name = (match name with
                | Some str -> str
                | None -> Core.default_name);
            state = Core.server_state_empty config;
            version = 0
        }

        let get_name { name } = name
    end

    let get_full_server_payload t =
        let open Server_payload in
        Full (
            t.Server.name,
            t.Server.version,
            Core.client_state_to_yojson (Core.client_state_from_server_state t.Server.state)
        )

    let process_message t msg =
        let (state, update) = Core.process_message t.Server.state t.Server.c msg in
        let version = t.Server.version + 1 in
        let open Server_payload in
        {
            t with
            Server.state; version
        },
        Update (
            t.Server.name,
            version,
            Core.update_to_yojson update
        )
end