type t

val create_chest : unit -> t
val more_skins : t -> bool
val open_chest : t -> Player.t -> unit
