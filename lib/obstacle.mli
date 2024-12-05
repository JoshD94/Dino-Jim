type t = {
  x : float;
  y : float;
  width : float;
  height : float;
  name : string;
}

val create : float -> string -> t
val init_obstacles : string -> int -> float -> unit -> t list

type obstacle_state = {
  spawned_count : int;
  total_count : int;
  passed_count : int;
}

val min_spacing : float ref
val obstacle_tracking : obstacle_state ref
