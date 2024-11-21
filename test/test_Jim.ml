open OUnit2
open Jim
open Jim.Skins
(* Player module Tests *)

let setup () =
  Raylib.init_window 1000 1000 "a";
  Raylib.set_target_fps 60

let () = setup ()

(* Test create makes separate instances of a player when called *)
let test_player_create () =
  "Create test"
  >::
  let p1 = Player.create () in
  let p2 = Player.create () in
  fun _ -> assert_bool "False" (p1 == p2 = false)

(* Test coins *)
let test_player_coins () =
  "Coins test"
  >::
  let p1 = Player.create () in
  let p2 = Player.create () in
  Player.add_coins p2 10;
  fun _ -> assert_bool "False" (Player.coins p1 = 10 && Player.coins p2 = 20)

(* Test skins *)
let test_player_skins () =
  "Skins test"
  >::
  let p1 = Player.create () in
  let p2 = Player.create () in
  Player.add_skin p2 SantaJim.draw;
  Player.select_skin p1 SantaJim.draw;
  Player.select_skin p2 SantaJim.draw;
  let b =
    (Player.current_skin p1) 0. 0. = DefaultSkin.draw 0. 0.
    && (Player.current_skin p2) 0. 0. = SantaJim.draw 0. 0.
    && List.map (fun x -> x 0. 0.) (Player.skins p1) = [ 0 ]
    && List.map (fun x -> x 0. 0.) (Player.skins p2) = [ 1; 0 ]
    && List.map (fun x -> x 0. 0.) (Player.buyable_skin_list p1) = [ 1 ]
    && List.map (fun x -> x 0. 0.) (Player.buyable_skin_list p2) = []
  in
  fun _ -> assert_bool "False" b

let tests =
  "Player Test Suite"
  >::: [ test_player_create (); test_player_coins (); test_player_skins () ]

let _ = run_test_tt_main tests
