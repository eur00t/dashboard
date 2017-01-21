open Core.Std;;
open Async.Std;;
open Message_processor;;

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

let () =
    Log.Global.set_level `Debug;
    let ws_server = App_websocket.start
        ~url: "ws://localhost:8081/api"
        ~handler: (Api_types.server_handler ~handler: (Dispatcher_server.Server.process_client_payload Dispatcher_server.server)) in

    (*let rec poll () =
        after (Time.Span.of_sec 5.0)
        >>= fun () ->
        App_websocket.send_all ws_server "ddd"
        >>= poll in

    ignore (poll ());*)
    ignore (subscribe_simple_printer (module XxlvChat));
    ignore (subscribe_simple_printer (module TestChat));
    ignore (subscribe_dispatcher (module TestChat) ws_server);

    (*XxlvChat.start ();*)
    TestChat.start ();
    never_returns (Scheduler.go ())