open Core.Std;;
open Async.Std;;

let msgs: string list ref = ref [];;

let process_message msg chat_name =
    let open Option in
    let time = Chat_message.get_time msg in
    Chat_message.get_from msg >>= fun from ->
    Chat_message.get_text msg >>| fun text ->
    printf "Chat: %s, Time: %d, From: %s, Text: %s\n" chat_name time from text;;