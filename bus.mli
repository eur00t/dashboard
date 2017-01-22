type 'a t

val on: 'a t -> ('a -> unit) -> int
val off: 'a t -> int -> unit
val emit: 'a t -> 'a -> unit
val create: 'a -> 'a t
val get_last: 'a t -> 'a