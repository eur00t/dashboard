type t = {
    id: int option;
    peer_id: int option;
    time: int;
    text: string option;
    from: string option;
    first_name: string option;
    last_name: string option;
    photo: string option;
};;

let create json =
    let open Yojson.Basic.Util in
    {
        id = json |> index 1 |> to_int_option;
        peer_id = json |> index 3 |> to_int_option;
        time = json |> index 4 |> to_int;
        text = json |> index 6 |> to_string_option;
        from = json |> index 7 |> member "from" |> to_string_option;
        first_name = None;
        last_name = None;
        photo = None;
    };;

let test_peer_id msg test =
    match msg.peer_id with
        | Some peer_id_-> Some (test peer_id_)
        | None -> None

let get_time msg = msg.time;;
let get_from msg = msg.from;;
let get_text msg = msg.text;;
let get_first_name msg = msg.first_name;;
let get_last_name msg = msg.last_name;;
let get_photo msg = msg.photo;;

let fill_user_info ~first_name ~last_name ~photo msg =
    {
        msg with
        first_name = Some first_name;
        last_name = Some last_name;
        photo = Some photo
    }