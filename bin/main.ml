open Jim.Menu
open Raylib
open Jim.Player
(* open Jim.Game *)
(* open Jim.Shop *)

(*let () = (* Initialize game with starting values *) let starting_level = 1 in
  let starting_length = 1 in let starting_speed = 1 in let gravity = 1.0 in let
  jump_force = -18. in

  init_game starting_level starting_length starting_speed gravity jump_force
  ()*)

let player = create ()

let () =
  run_menu player;
  close_window ()
(* let () = run_shop (); close_window () *)
