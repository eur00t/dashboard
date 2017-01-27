let table_to_pairs t =
    Hashtbl.fold begin
        fun key val_ l -> (key, val_) :: l
    end t []

let table_of_pairs pairs num =
    List.fold_left begin
        fun table (key, value) ->
            Hashtbl.add table key value;
            table
    end (Hashtbl.create num) pairs

module Hashtbl_ext = struct
    include Hashtbl

    type ('a, 'b) pairs_list = ('a * 'b) list [@@deriving yojson]

    let to_yojson a_to_yojson b_to_yojson t = pairs_list_to_yojson
        a_to_yojson b_to_yojson
        (table_to_pairs t)

    let of_yojson a_of_yojson b_of_yojson json =
        match pairs_list_of_yojson a_of_yojson b_of_yojson json with
            | Ok pairs -> Ok (table_of_pairs pairs 10)
            | Error _ as err -> err

    let keys t = fold (fun key _ res -> key:: res) t []
end

let queue_to_list q =
    let rec to_list q =
        if Queue.is_empty q then []
        else let elem = Queue.pop q in
        elem :: (to_list q)
    in
    List.rev (to_list (Queue.copy q))

let deoption_tuple4 t =
    match t with
        | (None, _, _, _) | (_, None, _, _)
        | (_, _, None, _) | (_,_, _, None) -> None
        | (Some a, Some b, Some c, Some d) -> Some (a, b, c, d)

let table_inc t key =
    let exists = Hashtbl.mem t key in
    Hashtbl.replace t key (if exists then (Hashtbl.find t key) + 1 else 1);
    exists

let table_dec t key =
    if Hashtbl.mem t key then
        let count = Hashtbl.find t key in
        if count = 1 then begin
            Hashtbl.remove t key;
            false
        end else begin
            Hashtbl.replace t key (count - 1);
            true
        end
    else false

let filter_some l =
    List.fold_left begin
        fun acc opt ->
            match opt with
                | Some a -> a :: acc
                | None -> acc
    end [] l

let print_i_two i = Printf.sprintf "%02u" i
let print_i i = Printf.sprintf "%u" i

let get_time_str t = (print_i_two t.Unix.tm_hour) ^ ":" ^ (print_i_two t.Unix.tm_min)

let get_date_str t =
    (print_i_two t.Unix.tm_mday) ^ "." ^
    (print_i_two (t.Unix.tm_mon + 1)) ^ "." ^
    (print_i (t.Unix.tm_year + 1900))

let get_timestamp t =
    (print_i (t.Unix.tm_year + 1900)) ^ "-" ^
    (print_i_two (t.Unix.tm_mon + 1)) ^ "-" ^
    (print_i_two t.Unix.tm_mday) ^ " " ^
    (print_i_two t.Unix.tm_hour) ^ ":" ^
    (print_i_two t.Unix.tm_min) ^ ":" ^
    (print_i_two t.Unix.tm_sec)
