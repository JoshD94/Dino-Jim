type position = {
  x : float;  (** x-coordinate *)
  y : float;  (** y-coordinate *)
  velocity_y : float;  (** vertical velocity *)
}
(** Represents a 2D position with vertical velocity *)

type game_state = {
  pos : position;  (** player position *)
  obstacles : Obstacle.t list;  (** list of active obstacles *)
  speed : float;  (** current game speed *)
  obstacles_passed : int;  (** number of obstacles cleared *)
  total_obstacles : int;  (** total obstacles in level *)
  completed : bool;  (** whether level is finished *)
  died : bool;  (** whether player died *)
  death_message : string;  (** message shown on death *)
}
(** Represents the complete state of the game *)

val update_position : position -> float -> float -> position
(** [update_position pos g jump_force] updates the position based on gravity [g]
    and jump force [jump_force]. The player can jump when on ground using Up or
    Space key. Position is constrained between ground level (350.0) and maximum
    jump height. *)

val get_death_message : string -> string
(** [get_death_message obstacle] returns a custom message describing how the
    player died from hitting the given [obstacle]. Returns "You Died!" for
    unknown obstacles. *)

val get_level_reward : int -> int
(** [get_level_reward level] returns the coin reward for completing the given
    [level]:
    - Levels 1-3 (Forest): 5 coins
    - Levels 4-6 (Snow): 10 coins
    - Levels 7-9 (Rock): 15 coins
    - Levels 10-12 (Lava): 20 coins *)
