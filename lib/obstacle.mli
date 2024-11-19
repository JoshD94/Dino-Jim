type t = ObstacleType.obstacle

val get_passed_count : unit -> int
val init_obstacles : string -> int -> unit -> t list
val update : t list -> float -> string -> t list
val check_collision : float -> float -> float -> float -> t list -> bool
val render : t list -> unit
