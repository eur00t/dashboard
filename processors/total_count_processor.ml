module Core = struct
    let name = "total"
    type server_state = int Queue.t
    type client_state = int [@@deriving yojson]
    type config = { interval_s: int } [@@deriving yojson]
    type update = int [@@deriving yojson]

    let client_state_empty _ = 0
    let server_state_empty _ = Queue.create ()
    let client_state_from_server_state q = Queue.length q
    let update_client_state count delta = count + delta
    let process_message q { interval_s } msg =
        let time = Chat_message.get_time msg in
        Queue.add time q;
        let delta = begin
            let delta_ref = ref 1 in
            while (
                (not (Queue.is_empty q)) &&
                (time - (Queue.peek q)) > interval_s
            ) do
                ignore (Queue.take q);
                delta_ref := !delta_ref - 1;
            done;
            !delta_ref
        end in
        q, delta
end

include Message_processor.Make_processor(Core)