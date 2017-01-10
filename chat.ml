open Core.Std;;
open Async.Std;;

let msgs: string list ref = ref [];;

let process_message ~id ~time ~text ~from =
    printf "Time: %d, From: %s, Text: %s\n" time from text;;