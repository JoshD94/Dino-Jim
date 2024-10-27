type t

val init_obstacles : unit -> t list
val update : t list -> float -> t list
val check_collision : float -> float -> float -> float -> t list -> bool
val render : t list -> unit
