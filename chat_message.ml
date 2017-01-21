type t = {
    id: int option;
    peer_id: int option;
    time: int;
    text: string option;
    from: string option
};;

let create json =
    let open Yojson.Basic.Util in
    {
        id = json |> index 1 |> to_int_option;
        peer_id = json |> index 3 |> to_int_option;
        time = json |> index 4 |> to_int;
        text = json |> index 6 |> to_string_option;
        from = json |> index 7 |> member "from" |> to_string_option
    };;

let test_peer_id msg test =
    match msg.peer_id with
        | Some peer_id_-> Some (test peer_id_)
        | None -> None

let get_time msg = msg.time;;
let get_from msg = msg.from;;
let get_text msg = msg.text;;