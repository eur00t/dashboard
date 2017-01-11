open Core.Std;;
open Async.Std;;

let xxlv_chat_id = 8;;
let test_chat_id = 18;;

module XxlvChat = Api_chat.Make_api_chat((val (Api_chat.make_single_chat_module xxlv_chat_id)));;
module TestChat = Api_chat.Make_api_chat((val (Api_chat.make_single_chat_module test_chat_id)));;

let () =
    XxlvChat.start ();
    TestChat.start ();
    never_returns (Scheduler.go ())