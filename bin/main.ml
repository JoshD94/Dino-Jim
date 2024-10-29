open Jim.Menu
open Raylib
(* open Jim.Game *)
(* open Jim.Shop *)

(*let () = (* Initialize game with starting values *) let starting_level = 1 in
  let starting_length = 1 in let starting_speed = 1 in let gravity = 1.0 in let
  jump_force = -18. in

  init_game starting_level starting_length starting_speed gravity jump_force
  ()*)

let () =
  run_menu ();
  close_window ()
(* let () = run_shop (); close_window () *)
