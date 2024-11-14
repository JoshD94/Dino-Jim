type position = {
  x : float;
  y : float;
  velocity_y : float;
}
(** Type representing the position of game entities *)

type game_state = {
  pos : position;
  obstacles : Obstacle.t list;
  speed : float;
}
(** Type representing the complete game state *)

val level : int
(** Current game level *)

val length : int
(** Length of the current level *)

val speed_up_multiplier : int
(** Speed multiplier for difficulty adjustment *)

val jump_force : float
(** Jump force for game physics*)

val gravity : float
(** Gravity constant for game physics*)

val init_game : int -> int -> int -> float -> float -> string -> unit -> unit
(** Initialize and start the game with given parameters
    @param lvl Game level
    @param len Game length
    @param spd Game speed multiplier
    @param g Gravity constant
    @param jf Jump force constant
    @param biome Biome type *)

val update_position : position -> float -> float -> position
(** Update position based on input
    @param pos Current position
    @param g Gravity constant
    @param jf Jump force constant
    @return New position *)

val render : game_state -> unit
(** Render game state
    @param state Current game state *)
