open Message_processor;;

module Log = Async.Std.Log.Global

let xxlv_chat_id = 8;;
let test_chat_id = 18;;

module XxlvChat = Api_chat.Make_api_chat((val (Api_chat.make_single_chat_module
    xxlv_chat_id
    ~name:"XXLv")));;
module TestChat = Api_chat.Make_api_chat((val (Api_chat.make_single_chat_module
    test_chat_id
    ~name:"Test")));;

let subscribe_simple_printer (module ChatModule: Api_chat.Api_chat) =
    ChatModule.subscribe (fun msg ->
        ignore ((Chat.process_message msg ChatModule.Config.name): unit option);
        ());;

let subscribe_dispatcher (module ChatModule: Api_chat.Api_chat) ws_server =
    ChatModule.subscribe (fun msg ->
        Dispatcher_server.Server.process_message
            ws_server
            Dispatcher_server.server
            msg);;

let dump () =
    try
        let dump_filename = Array.get Sys.argv 1 in
        Printf.printf "Saving state to file %s\n" dump_filename;
        Dispatcher_server.Server.dump_to_file
            Dispatcher_server.server
            dump_filename;
    with
        Invalid_argument _ -> ()

let dump_on_sig code =
    Printf.printf "Received signal (%d).\n" code;
    dump ();
    exit 0

let read_dump () =
    try
        let dump_filename = Array.get Sys.argv 1 in
        Dispatcher_server.Server.read_dump_from_file
            Dispatcher_server.server
            dump_filename;
        Log.info "Dump was loaded successfully from file \"%s\"" dump_filename;
    with
        | Invalid_argument _ -> ()
        | Sys_error msg -> Log.error "Error while reading dump: %s" msg;;

let () =
    Log.set_level `Info;
    read_dump ();
    let ws_server = App_websocket.start
        ?cert_file: App_config.cert_file
        ?key_file: App_config.key_file
        ~url: (App_config.ws_host ^ "/api")
        ~handler: (Api_types.server_handler ~handler: (Dispatcher_server.Server.process_client_payload Dispatcher_server.server))
        () in

    Sys.set_signal Sys.sigint (Sys.Signal_handle dump_on_sig);
    Sys.set_signal Sys.sigpipe (Sys.Signal_handle (fun _ ->
        Log.info "Received SIGPIPE, should ignore";
    ));
    (*let rec poll () =
        after (Time.Span.of_sec 5.0)
        >>= fun () ->
        App_websocket.send_all ws_server "ddd"
        >>= poll in

    ignore (poll ());*)
    ignore (subscribe_simple_printer (module XxlvChat));
    ignore (subscribe_simple_printer (module TestChat));
    ignore (subscribe_dispatcher (module XxlvChat) ws_server);
    ignore (subscribe_dispatcher (module TestChat) ws_server);

    XxlvChat.start ();
    TestChat.start ();
    try
        Core.Std.never_returns (Async.Std.Scheduler.go ~raise_unhandled_exn: true ())
    with
        exn ->
            Printf.printf "Exception in Async scheduler: %s\n" (Core.Std.Exn.to_string exn);
            dump ();
            exit 1