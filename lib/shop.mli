val button_height : float
val button_width : float
val padding : float
val grid_cols : int
val num_items : int
val buyable_items : (string * int) array
val bought_items : bool array

type player_state = {
  mutable coins : int;
  mutable bought_items : string list;
}

val player : player_state
val run_shop : Player.t -> unit
