type t

val create: Yojson.Basic.json -> t

val test_peer_id: t -> (int -> bool) -> bool option

val get_time: t -> int
val get_from: t -> string option
val get_text: t -> string option