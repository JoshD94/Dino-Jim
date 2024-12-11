type t

val complete_onboarding : t -> unit
val has_seen_onboarding : t -> bool
val create_player : unit -> t
val add_coins : t -> int -> unit
val add_skin : t -> (float -> float -> int) -> unit
val skins : t -> (float -> float -> int) list
val coins : t -> int
val buyable_skin_list : t -> (float -> float -> int) list
val current_skin : t -> float -> float -> int
val select_skin : t -> (float -> float -> int) -> unit
val has_skin : t -> (float -> float -> int) -> bool

(* New level progress functions *)
val is_level_unlocked : t -> int -> bool
val complete_level : t -> int -> unit
val get_completed_levels : t -> int list
val set_save_file : string -> unit
