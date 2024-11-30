type position = {
  x : float;
  y : float;
  velocity_y : float;
}

type game_state = {
  pos : position;
  obstacles : ObstacleType.obstacle list;
  speed : float;
  obstacles_passed : int;
  total_obstacles : int;
  completed : bool;
  died : bool;
  death_message : string;
}

val level : int
val speed_up_multiplier : int
val jump_force : float
val gravity : float

val init_game :
  int -> int -> int -> float -> float -> string -> Player.t -> unit -> unit

val update_position : position -> float -> float -> position
val render : game_state -> Player.t -> unit
val get_death_message : string -> string

val get_collision_obstacle :
  float ->
  float ->
  float ->
  float ->
  ObstacleType.obstacle list ->
  string option
