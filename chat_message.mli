type t

val create: Yojson.Basic.json -> t

val test_peer_id: t -> (int -> bool) -> bool option

val get_time: t -> int
val get_from: t -> string option
val get_text: t -> string option
val get_first_name: t -> string option
val get_last_name: t -> string option
val get_photo: t -> string option

val fill_user_info: first_name: string ->
last_name: string -> photo: string -> t -> t
