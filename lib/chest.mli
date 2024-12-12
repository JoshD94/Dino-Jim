type t
(** The type of a chest *)

val create_chest : unit -> t
(** [create_chest ())] creates a chest the contains all the possible skins to be
    obtainted from chests *)

val more_skins : t -> bool
(** [more_skins chest] returns true if [chest] has more skins to be obtained *)

val open_chest : t -> Player.t -> unit
(** [open_chest chest player] gets the next available skin from [chest] and
    gives it to [player]. Does nothing if [chest] has no more skins *)
