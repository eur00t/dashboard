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
        ())

let () =
    ignore (subscribe_simple_printer (module XxlvChat));
    ignore (subscribe_simple_printer (module TestChat));

    XxlvChat.start ();
    TestChat.start ();
    never_returns (Scheduler.go ())