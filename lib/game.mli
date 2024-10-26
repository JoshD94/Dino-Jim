type position = {
  x : float;
  y : float;
  velocity_y : float;
}
(** Type representing the position of game entities *)

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

val init_game : int -> int -> int -> float -> float -> unit -> unit
(** Initialize and start the game with given parameters
    @param lvl Current level number
    @param len Level length
    @param speed_mul Speed multiplier
    @param g Gravity constant
    @param jf Jump force constant *)

val update_position : position -> float -> float -> position
(** Update position based on input
    @param pos Current position
    @param g Gravity constant
    @param jf Jump force constant
    @return New position *)

val render : position -> unit
(** Render game state
    @param pos Current position *)
