type t

val create : unit -> t
val add_coins : t -> int -> unit
val add_skin : t -> string -> unit
val add_powerup : t -> string -> unit
val skins : t -> string list
val coins : t -> int
