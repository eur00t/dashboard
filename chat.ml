open Core.Std;;
open Async.Std;;

let msgs: string list ref = ref [];;

let process_message msg chat_name =
    let open Option in
    let time = Chat_message.get_time msg in
    Chat_message.get_first_name msg >>= fun first_name ->
    Chat_message.get_last_name msg >>= fun last_name ->
    Chat_message.get_text msg >>| fun text ->
    printf "Chat: %s, Time: %d, From: %s, Text: %s\n" chat_name time (first_name ^ " " ^ last_name) text;;