type t

val create : unit -> t
val add_coins : t -> int -> unit
val add_skin : t -> (float -> float -> unit) -> unit
val add_powerup : t -> string -> unit
val skins : t -> (float -> float -> unit) list
val has_powerup : t -> string -> bool
val coins : t -> int
val buyable_skin_list : t -> (float -> float -> unit) list
val current_skin : t -> float -> float -> unit
val select_skin : t -> (float -> float -> unit) -> unit
