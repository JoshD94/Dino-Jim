(* main.ml *)
open Jim.Game

let () =
  (* Initialize game with starting values *)
  let starting_level = 1 in
  let starting_length = 1 in
  let starting_speed = 1 in
  let gravity = 1.0 in
  let jump_force = -18. in

  init_game starting_level starting_length starting_speed gravity jump_force ()
