open Core.Std;;
open Async.Std;;

module type Api_chat_config = sig
    val name: string
    val test: int -> bool
end;;

module type Api_chat = sig
    module Config: Api_chat_config
    val start: unit -> unit
    val subscribe: (Chat_message.t -> unit) -> (Chat_message.t -> unit) Core.Std.Bus.Subscriber.t
end;;

module Make_api_chat (Config: Api_chat_config): Api_chat = struct
    type get_long_poll_server_response = {
        key: string;
        server: string;
        ts: int
    } [@@deriving of_yojson { strict = false }]

    type user_info = {
        first_name: string;
        last_name: string;
        photo_50: string
    } [@@deriving of_yojson { strict = false }]

    type users_info = user_info list [@@deriving of_yojson { strict = false }]

    module Config = Config;;

    let bus =
    let open Core.Std.Bus in
    create [%here] Callback_arity.Arity1
        ~allow_subscription_after_first_write:false
        ~on_callback_raise: (fun err -> ());;

    let get_long_poll_server () =
        let open Deferred.Or_error.Monad_infix in
        Api.do_request "messages.getLongPollServer" [("ssl", "1")]
        >>= fun response -> return (Util.result_to_or_error (get_long_poll_server_response_of_yojson response));;

    type long_poll_message =
        | Reconnect of int
        | Restart of int option
        | Data of int * Yojson.Basic.json;;

    type update =
        | Message of Yojson.Basic.json
        | Other of Yojson.Basic.json;;

    let get_update_variant json =
        let open Yojson.Basic.Util in
        match index 0 json |> to_int with
            | 4 -> Message json
            | _ -> Other json;;

    let user_info_cache = String.Table.create ()

    let get_user_info id =
        let open Deferred.Or_error.Monad_infix in
        match String.Table.find user_info_cache id with
            | Some response ->
                Deferred.Or_error.ok_unit
                >>= fun _ ->
                return (Ok response)
            | None ->
                Api.do_request "users.get" [("user_ids", id); ("fields", "photo_50")]
                >>= fun response ->
                String.Table.set user_info_cache ~key: id ~data: response;
                return (Ok response)

    let fill_user_info msg =
        let open Deferred.Or_error.Monad_infix in
        Deferred.Or_error.ok_unit
        >>= fun _ ->
            (match Chat_message.get_from msg with
                | Some str -> return (Ok str)
                | None -> Deferred.Or_error.error_string "From field is missing");
        >>= fun from ->
        get_user_info from;
        >>= fun response ->
        return (Util.result_to_or_error (users_info_of_yojson response));
        >>= function
            | [user] -> return (Ok user)
            | [] | _ -> Deferred.Or_error.error_string "Wrong API response";
        >>= fun user ->
        return (Ok (Chat_message.fill_user_info
            ~first_name: user.first_name
            ~last_name: user.last_name
            ~photo: user.photo_50
            msg))

    let process_chat_message msg =
        fill_user_info msg
        >>= begin
            function
                | Ok msg -> return msg
                | Error _ -> return msg
        end
        >>= fun msg ->
        Core.Std.Bus.write bus msg;
        return ();;

    let process_chat_message msg =
        match Chat_message.test_peer_id msg Config.test with
            | Some true -> process_chat_message msg
            | Some false | None -> return ();;

    let process_update_variant = function
        | Message json -> process_chat_message (Chat_message.create json)
        | Other _ -> return ();;

    let process_updates updates =
        let open Yojson.Basic.Util in
        ignore
            ((updates |> to_list)
            |> List.map ~f:get_update_variant
            |> List.map ~f:process_update_variant);

        (*printf "Updates: %s\n" (Yojson.Basic.pretty_to_string updates);*)
        ();;

    let rec initiate_long_poll_connection key server ts =
        Api.do_request_inner ("https://" ^ server) [
            ("act", "a_check");
            ("key", key);
            ("ts", string_of_int ts);
            ("wait", "25");
            ("mode", "2");
            ("version", "1")
        ]
        >>= fun json ->
            let json = Yojson.Safe.to_basic json in
            let open Yojson.Basic.Util in
            let failed = json |> member "failed" in
            let updates = json |> member "updates" in
            let ts_ = json |> member "ts" in
            let res = if failed <> `Null then
                let failed_code = failed |> to_int in
                match failed_code with
                    | 1 -> Reconnect (ts_ |> to_int)
                    | 2 -> Restart (Some ts)
                    | 3 | _ -> Restart None
                else Data ((ts_ |> to_int), updates) in
            return res
        >>= function
            | Reconnect ts -> initiate_long_poll_connection key server ts
            | Restart Some ts -> connect_to_long_poll_server ~ts_override:ts ()
            | Restart None -> connect_to_long_poll_server ()
            | Data (ts, updates) ->
                process_updates updates;
                initiate_long_poll_connection key server ts

    and connect_to_long_poll_server ?ts_override () =
        let open Deferred.Or_error.Monad_infix in
        get_long_poll_server ()
        >>= fun response ->
        let { key; server; ts = ts_from_server } = response in
        printf "key: %s, server: %s, ts: %d\n" key server ts_from_server;
        let ts = match ts_override with
            | Some ts -> ts
            | None -> ts_from_server in
        initiate_long_poll_connection key server ts;;

    let print_deffered_string def =
        def >>| fun str ->
        printf "%s" str;;

    let print_defferred_or_error_json def =
        def
        >>| (function
            | Ok json -> Yojson.Basic.to_string json
            | Error error -> Error.to_string_hum error)
        >>| fun str ->
        printf "%s" str;;

    let start () =
        ignore ((print_defferred_or_error_json (connect_to_long_poll_server ())): unit Deferred.t)

    let subscribe callback =
        Core.Std.Bus.subscribe_exn (Core.Std.Bus.read_only bus) [%here] ~f:callback;;
end;;

let get_chat_id id = 2000000000 + id;;

let make_single_chat_module ?(name="default") chat_id =
    (module struct
        let test peer_id = (peer_id = get_chat_id chat_id)
        let name = name
    end: Api_chat_config);;