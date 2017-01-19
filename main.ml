open Core.Std;;
open Async.Std;;

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

let handler msg =
    let open Api_types in
    match msg with
        | Initial_request num -> Initial_response { Initial_response.test = num + 1 };;

let () =
    Log.Global.set_level `Debug;
    let ws_server = App_websocket.start
        ~url: "ws://localhost:8081/api"
        ~handler: (Api_types.server_handler ~handler: handler) in

    (*let rec poll () =
        after (Time.Span.of_sec 5.0)
        >>= fun () ->
        App_websocket.send_all ws_server "ddd"
        >>= poll in

    ignore (poll ());*)
    ignore (subscribe_simple_printer (module XxlvChat));
    ignore (subscribe_simple_printer (module TestChat));

    (*XxlvChat.start ();*)
    TestChat.start ();
    never_returns (Scheduler.go ())