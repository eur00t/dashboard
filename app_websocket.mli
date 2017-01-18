type t

val start: url: string -> handler: (string -> string) -> t;;

val send_all: t -> string -> unit Async.Std.Deferred.t;;