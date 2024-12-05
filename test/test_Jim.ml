open OUnit2
open Jim.Game
open Jim
open Jim.Skins
open Jim.Player

(* Setup for testing Skins *)
let setup () =
  Raylib.init_window 1000 1000 "a";
  Raylib.set_target_fps 60

let () = setup ()

(* Helper function to compare position record type *)
let assert_position_equal (expected : position) (actual : position) =
  assert_equal expected.x actual.x;
  assert_equal expected.y actual.y;
  assert_equal expected.velocity_y actual.velocity_y

(* Test create makes separate instances of a player when called *)
let test_player_create () =
  "Create test"
  >::
  let p1 = create_player () in
  let p2 = create_player () in
  fun _ -> assert_bool "Create False" (p1 == p2 = false)

(* Test coins *)
let test_player_coins () =
  "Coins test"
  >::
  let p1 = create_player () in
  let p2 = create_player () in
  Player.add_coins p2 10;
  fun _ ->
    assert_bool "Coins False" (Player.coins p1 = 10 && Player.coins p2 = 20)

(* Test skin id *)
let test_player_skins_id () =
  "Skins id test"
  >::
  let p1 = create_player () in
  let p2 = create_player () in
  select_skin p1 SantaJim.draw;
  select_skin p2 DefaultSkin.draw;
  add_skin p2 OrangeJim.draw;
  add_skin p2 RedJim.draw;
  add_skin p2 GreenJim.draw;
  add_skin p2 BlueJim.draw;
  add_skin p2 DarthJim.draw;
  add_skin p2 InvisibleJim.draw;
  let b =
    (Player.current_skin p1) 0. 0. = SantaJim.draw 0. 0.
    && (Player.current_skin p2) 0. 0. = DefaultSkin.draw 0. 0.
    && List.map (fun x -> x 0. 0.) (Player.skins p1) = [ 0; 1; 2 ]
    && List.map (fun x -> x 0. 0.) (Player.skins p2)
       = [ 6; 8; 5; 3; 4; 7; 0; 1; 2 ]
    && List.map (fun x -> x 0. 0.) (Player.buyable_skin_list p1)
       = [ 3; 4; 7; 5; 6; 8 ]
    && List.map (fun x -> x 0. 0.) (Player.buyable_skin_list p2) = []
  in
  fun _ -> assert_bool "Skins id False" b

(* Test Buy skins *)
let test_buy_skins () =
  "Buy skins test"
  >::
  let p = create_player () in
  add_skin p DefaultSkin.draw;
  let a =
    List.map (fun x -> x 0. 0.) (buyable_skin_list p) = [ 3; 4; 7; 5; 6; 8 ]
  in
  add_skin p SantaJim.draw;
  let b =
    List.map (fun x -> x 0. 0.) (buyable_skin_list p) = [ 3; 4; 7; 5; 6; 8 ]
  in
  add_skin p OrangeJim.draw;
  let c =
    List.map (fun x -> x 0. 0.) (buyable_skin_list p) = [ 3; 4; 5; 6; 8 ]
  in
  add_skin p BlueJim.draw;
  let d = List.map (fun x -> x 0. 0.) (buyable_skin_list p) = [ 3; 4; 6; 8 ] in
  add_skin p InvisibleJim.draw;
  let e = List.map (fun x -> x 0. 0.) (buyable_skin_list p) = [ 3; 4; 8 ] in
  add_skin p DarthJim.draw;
  let f = List.map (fun x -> x 0. 0.) (buyable_skin_list p) = [ 3; 4 ] in
  add_skin p GreenJim.draw;
  let g = List.map (fun x -> x 0. 0.) (buyable_skin_list p) = [ 4 ] in
  add_skin p RedJim.draw;
  let h = List.map (fun x -> x 0. 0.) (buyable_skin_list p) = [] in
  ();
  fun _ -> assert_bool "Buy skins false" (a && b && c && d && e && f && g && h)

(* Test add skins *)
let test_add_skins () =
  "Add skins test"
  >::
  let p = create_player () in
  add_skin p DefaultSkin.draw;
  let a = List.map (fun x -> x 0. 0.) (skins p) = [ 0; 1; 2 ] in
  add_skin p SantaJim.draw;
  let b = List.map (fun x -> x 0. 0.) (skins p) = [ 0; 1; 2 ] in
  add_skin p OrangeJim.draw;
  let c = List.map (fun x -> x 0. 0.) (skins p) = [ 7; 0; 1; 2 ] in
  add_skin p BlueJim.draw;
  let d = List.map (fun x -> x 0. 0.) (skins p) = [ 5; 7; 0; 1; 2 ] in
  add_skin p InvisibleJim.draw;
  let e = List.map (fun x -> x 0. 0.) (skins p) = [ 6; 5; 7; 0; 1; 2 ] in
  add_skin p DarthJim.draw;
  let f = List.map (fun x -> x 0. 0.) (skins p) = [ 8; 6; 5; 7; 0; 1; 2 ] in
  add_skin p GreenJim.draw;
  let g = List.map (fun x -> x 0. 0.) (skins p) = [ 3; 8; 6; 5; 7; 0; 1; 2 ] in
  add_skin p RedJim.draw;
  let h =
    List.map (fun x -> x 0. 0.) (skins p) = [ 4; 3; 8; 6; 5; 7; 0; 1; 2 ]
  in
  ();
  fun _ -> assert_bool "Buy skins false" (a && b && c && d && e && f && g && h)

(* Test Skin select *)
let test_select_skin () =
  "Skin select test"
  >::
  let p = create_player () in
  select_skin p DefaultSkin.draw;
  let a = (current_skin p) 0. 0. = DefaultSkin.draw 0. 0. in
  select_skin p SantaJim.draw;
  let b =
    (current_skin p) 0. 0. = SantaJim.draw 0. 0.
    && (current_skin p) 0. 0. <> DefaultSkin.draw 0. 0.
  in
  select_skin p AngryJim.draw;
  let c = (current_skin p) 0. 0. = AngryJim.draw 0. 0. in
  select_skin p OrangeJim.draw;
  let d = (current_skin p) 0. 0. = AngryJim.draw 0. 0. in
  select_skin p RedJim.draw;
  let e = (current_skin p) 0. 0. = AngryJim.draw 0. 0. in
  select_skin p GreenJim.draw;
  let f = (current_skin p) 0. 0. = AngryJim.draw 0. 0. in
  select_skin p InvisibleJim.draw;
  let g = (current_skin p) 0. 0. = AngryJim.draw 0. 0. in
  select_skin p DarthJim.draw;
  let h = (current_skin p) 0. 0. = AngryJim.draw 0. 0. in
  select_skin p BlueJim.draw;
  add_skin p OrangeJim.draw;
  add_skin p RedJim.draw;
  add_skin p GreenJim.draw;
  add_skin p BlueJim.draw;
  add_skin p DarthJim.draw;
  add_skin p InvisibleJim.draw;
  let i = (current_skin p) 0. 0. = AngryJim.draw 0. 0. in
  select_skin p OrangeJim.draw;
  let j = (current_skin p) 0. 0. = OrangeJim.draw 0. 0. in
  select_skin p RedJim.draw;
  let k = (current_skin p) 0. 0. = RedJim.draw 0. 0. in
  select_skin p GreenJim.draw;
  let l = (current_skin p) 0. 0. = GreenJim.draw 0. 0. in
  select_skin p InvisibleJim.draw;
  let m = (current_skin p) 0. 0. = InvisibleJim.draw 0. 0. in
  select_skin p DarthJim.draw;
  let n = (current_skin p) 0. 0. = DarthJim.draw 0. 0. in
  select_skin p BlueJim.draw;
  let o = (current_skin p) 0. 0. = BlueJim.draw 0. 0. in
  ();
  fun _ ->
    assert_bool "Skin select false"
      (a && b && c && d && e && f && g && h && i && j && k && l && m && n && o)

(* Test Powerups *)
let test_powerups () =
  "Powerups test"
  >::
  let p = create_player () in
  add_powerup p "";
  let a = has_powerup p "" = false in
  add_powerup p "Jump";
  let b = has_powerup p "Faster" = false in
  add_powerup p "Faster";
  let c = has_powerup p "Faster" in
  add_powerup p "More coins";
  let d = has_powerup p "Jump" in
  ();
  let e = has_powerup p "More coins" in
  ();
  fun _ -> assert_bool "Powerups test false" (a && b && c && d && e)

(* Test Jim.Player *)
let player_tests =
  "Test suite for Player"
  >::: [
         test_player_create ();
         test_player_skins_id ();
         test_player_coins ();
         test_buy_skins ();
         test_add_skins ();
         test_powerups ();
         test_select_skin ();
       ]

(* Test Jim.Obstacle.create *)
let obstacle_create_tests =
  "test suite for Obstacle.create"
  >::: [
         ( "test create cactus in grass biome" >:: fun _ ->
           Random.init 42;
           (* Set fixed seed for deterministic tests *)
           let obs = Jim.Obstacle.create 100.0 "grass" in
           assert_bool "x position in valid range" (obs.x >= 100.0);
           assert_bool "name is valid"
             (List.mem obs.name [ "cactus"; "rock"; "tree"; "bush" ]) );
         ( "test create rock in rock biome" >:: fun _ ->
           Random.init 43;
           let obs = Jim.Obstacle.create 200.0 "rock" in
           assert_bool "x position in valid range" (obs.x >= 200.0);
           assert_bool "name is valid"
             (List.mem obs.name [ "rock"; "boulder"; "wall"; "evergreen" ]) );
         ( "test create snowman in snow biome" >:: fun _ ->
           Random.init 44;
           let obs = Jim.Obstacle.create 300.0 "snow" in
           assert_bool "x position in valid range" (obs.x >= 300.0);
           assert_bool "name is valid"
             (List.mem obs.name [ "snowman"; "ice"; "rock"; "evergreen" ]) );
         ( "test create lava in lava biome" >:: fun _ ->
           Random.init 45;
           let obs = Jim.Obstacle.create 400.0 "lava" in
           assert_bool "x position in valid range" (obs.x >= 400.0);
           assert_bool "name is valid"
             (List.mem obs.name [ "lava"; "asteroid"; "fire"; "spike" ]) );
         ( "test create defaults to grass biome" >:: fun _ ->
           Random.init 46;
           let obs = Jim.Obstacle.create 500.0 "unknown_biome" in
           assert_bool "x position in valid range" (obs.x >= 500.0);
           assert_bool "name is valid"
             (List.mem obs.name [ "cactus"; "rock"; "tree"; "bush" ]) );
       ]

(* Test Jim.Obstacle.init_obstacles *)
let obstacle_init_tests =
  "test suite for Obstacle.init_obstacles"
  >::: [
         ( "test init grass biome obstacles" >:: fun _ ->
           Random.init 47;
           let obstacles = Jim.Obstacle.init_obstacles "grass" 3 200.0 () in
           assert_equal 1 (List.length obstacles);
           let first = List.hd obstacles in
           assert_bool "first obstacle beyond safe distance" (first.x >= 1200.0)
         );
         ( "test init rock biome obstacles" >:: fun _ ->
           Random.init 48;
           let obstacles = Jim.Obstacle.init_obstacles "rock" 5 300.0 () in
           assert_equal 1 (List.length obstacles);
           let first = List.hd obstacles in
           assert_bool "first obstacle beyond safe distance" (first.x >= 1200.0)
         );
       ]

(* Test get_death_message pattern matching *)
let death_message_tests =
  "test suite for get_death_message"
  >::: [
         ( "test cactus death" >:: fun _ ->
           assert_equal "Jim got pricked by a cactus."
             (get_death_message "cactus") );
         ( "test rock death" >:: fun _ ->
           assert_equal "Jim stumbled on a rock." (get_death_message "rock") );
         ( "test tree death" >:: fun _ ->
           assert_equal "Jim ran straight into a tree."
             (get_death_message "tree") );
         ( "test bush death" >:: fun _ ->
           assert_equal "Jim got tangled in a bush." (get_death_message "bush")
         );
         ( "test snowman death" >:: fun _ ->
           assert_equal "Jim doesn't like snowmen."
             (get_death_message "snowman") );
         ( "test ice death" >:: fun _ ->
           assert_equal "Jim slipped on ice." (get_death_message "ice") );
         ( "test evergreen death" >:: fun _ ->
           assert_equal "Jim slammed into an evergreen."
             (get_death_message "evergreen") );
         ( "test boulder death" >:: fun _ ->
           assert_equal "Jim nearly died from a boulder."
             (get_death_message "boulder") );
         ( "test wall death" >:: fun _ ->
           assert_equal "Jim hit a wall." (get_death_message "wall") );
         ( "test lava death" >:: fun _ ->
           assert_equal "Jim is not lava resistant." (get_death_message "lava")
         );
         ( "test asteroid death" >:: fun _ ->
           assert_equal "Jim stubbed his toe on an asteroid."
             (get_death_message "asteroid") );
         ( "test fire death" >:: fun _ ->
           assert_equal "Jim is no fireman." (get_death_message "fire") );
         ( "test spike death" >:: fun _ ->
           assert_equal "Jim got impaled on a spike."
             (get_death_message "spike") );
         ( "test unknown death" >:: fun _ ->
           assert_equal "You Died!" (get_death_message "unknown") );
       ]

(* Test update_position *)
let update_position_tests =
  "test suite for update_position"
  >::: [
         ( "test initial position on ground" >:: fun _ ->
           let pos = { x = 100.0; y = 350.0; velocity_y = 0.0 } in
           let new_pos = update_position pos 0.8 (-20.0) in
           let expected = { x = 100.0; y = 350.0; velocity_y = 0.0 } in
           assert_position_equal expected new_pos );
         ( "test mid-jump position" >:: fun _ ->
           let pos = { x = 100.0; y = 300.0; velocity_y = -10.0 } in
           let new_pos = update_position pos 0.8 (-20.0) in
           assert_equal 100.0 new_pos.x;
           assert_bool "new y position is higher" (new_pos.y < 300.0);
           assert_bool "velocity is affected by gravity"
             (new_pos.velocity_y > -10.0) );
         ( "test maximum jump height" >:: fun _ ->
           let pos = { x = 100.0; y = 150.0; velocity_y = -20.0 } in
           let new_pos = update_position pos 0.8 (-20.0) in
           assert_equal 100.0 new_pos.x;
           assert_bool "y position respects jump height limit"
             (new_pos.y >= 150.0) );
         ( "test falling position" >:: fun _ ->
           let pos = { x = 100.0; y = 200.0; velocity_y = 5.0 } in
           let new_pos = update_position pos 0.8 (-20.0) in
           assert_equal 100.0 new_pos.x;
           assert_bool "y position increases while falling" (new_pos.y > 200.0);
           assert_bool "velocity increases while falling"
             (new_pos.velocity_y > 5.0) );
         ( "test landing position" >:: fun _ ->
           let pos = { x = 100.0; y = 349.0; velocity_y = 5.0 } in
           let new_pos = update_position pos 0.8 (-20.0) in
           let expected = { x = 100.0; y = 350.0; velocity_y = 0.0 } in
           assert_position_equal expected new_pos );
       ]

(* Test get_biome_for_level *)
(* let biome_tests = "test suite for get_biome_for_level" >::: [ ( "test grass
   biome levels" >:: fun _ -> assert_equal "grass" (get_biome_for_level 1);
   assert_equal "grass" (get_biome_for_level 2); assert_equal "grass"
   (get_biome_for_level 3) ); ( "test rock biome levels" >:: fun _ ->
   assert_equal "rock" (get_biome_for_level 4); assert_equal "rock"
   (get_biome_for_level 5); assert_equal "rock" (get_biome_for_level 6) ); (
   "test snow biome levels" >:: fun _ -> assert_equal "snow"
   (get_biome_for_level 7); assert_equal "snow" (get_biome_for_level 8);
   assert_equal "snow" (get_biome_for_level 9) ); ( "test lava biome levels" >::
   fun _ -> assert_equal "lava" (get_biome_for_level 10); assert_equal "lava"
   (get_biome_for_level 11); assert_equal "lava" (get_biome_for_level 12) );
   ] *)

let () =
  run_test_tt_main
    ("all_tests"
    >::: [
           player_tests;
           obstacle_create_tests;
           obstacle_init_tests;
           death_message_tests;
           update_position_tests;
           (* biome_tests; *)
         ])
