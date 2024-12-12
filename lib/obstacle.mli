type t = {
  x : float;  (** x-coordinate *)
  y : float;  (** y-coordinate *)
  width : float;  (** obstacle width *)
  height : float;  (** obstacle height *)
  name : string;  (** obstacle type name *)
}
(** Represents an obstacle in the game *)

val create : float -> string -> t
(** [create x biome] creates a new random obstacle at position [x] for the given
    [biome] (grass/snow/rock/lava). The obstacle's dimensions are chosen
    randomly based on the biome. *)

val init_obstacles : string -> int -> float -> unit -> t list
(** [init_obstacles biome total distance ()] initializes the first obstacle for
    a new level with [total] obstacles in [biome], spaced by [distance]. Returns
    a singleton list with the first obstacle. *)

type obstacle_state = {
  spawned_count : int;  (** number of obstacles spawned *)
  total_count : int;  (** total obstacles in level *)
  passed_count : int;  (** obstacles successfully passed *)
}
(** Tracks the state of obstacles in the current level *)

val min_spacing : float ref
(** Minimum spacing between obstacles *)

val obstacle_tracking : obstacle_state ref
(** Current obstacle tracking state *)

val get_biome_for_level : int -> string
(** Returns biome for specified level*)
