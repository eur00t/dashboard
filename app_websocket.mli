type t

val start: ?cert_file: string -> ?key_file: string -> url: string -> handler: (string -> string) -> unit -> t;;

val send_all: t -> string -> unit Async.Std.Deferred.t;;