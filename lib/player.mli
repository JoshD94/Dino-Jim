type t
(** The type of a player *)

val complete_onboarding : t -> unit
(** [complete_onboarding player] marks that [player] has completed the
    onboarding process. *)

val has_seen_onboarding : t -> bool
(** [has_seen_onboarding player] checks if [player] has seen the onboarding
    screen. *)

val create_player : unit -> t
(** [create_player ()] creates a brand new player instance with all values
    initialized. If a csv save file has player data then that data will be
    loaded into the player instance instead of the default values. *)

val add_coins : t -> int -> unit
(** [add_coins player num] adds [num] amount of coins to [player]. *)

val add_skin : t -> (float -> float -> int) -> unit
(** [add_skin player skin] adds the skin [skin] to the player [player]'s skin
    list. Does nothing if the player already has the skin. *)

val skins : t -> (float -> float -> int) list
(** [skins player] returns the list of skins that [player] owns. *)

val coins : t -> int
(** [coins player] returns the amount of coins that [player] has. *)

val buyable_skin_list : t -> (float -> float -> int) list
(** [buyable_skin_list player] returns the list of skins that are currently
    buyable in the shop for the player*)

val current_skin : t -> float -> float -> int
(** [current_skin player] returns the skin [player] currently has selected. *)

val select_skin : t -> (float -> float -> int) -> unit
(** [select_skin player skin] changes [player]'s current skin and change it to
    [skin]. Does nothing if the player doesn't own the skin. *)

val has_skin : t -> (float -> float -> int) -> bool
(** [has_skin player skin] returns true if the player owns [skin] or false
    otherwise. *)

val is_level_unlocked : t -> int -> bool
(** [is_level_unlocked player level] returns if [player] has level [level]
    unlocked in the menu. *)

val complete_level : t -> int -> unit
(** [complete_level player level] sets the level [level] to complete for
    [player]. *)

val get_completed_levels : t -> int list
(** [get_completed_levels player] returns the list of levels that [player] has
    completed *)

val set_save_file : string -> unit
(** [set_save_file file_path] sets the filepath for the savefile of any player
    to [file_path] *)
