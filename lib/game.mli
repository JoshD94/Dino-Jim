type position = {
  x : float;
  y : float;
  velocity_y : float;
}

type game_state = {
  pos : position;
  obstacles : Obstacle.t list;
  speed : float;
  obstacles_passed : int;
  total_obstacles : int;
  completed : bool;
  died : bool;
  death_message : string;
}

val update_position : position -> float -> float -> position
val get_death_message : string -> string
val get_level_reward : int -> int
