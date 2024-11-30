open OUnit2
open Jim.ObstacleType
open Jim.Game
open Jim.Menu

(* Helper function to compare position record type *)
let assert_position_equal expected actual =
  assert_equal expected.x actual.x;
  assert_equal expected.y actual.y;
  assert_equal expected.velocity_y actual.velocity_y

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
let biome_tests =
  "test suite for get_biome_for_level"
  >::: [
         ( "test grass biome levels" >:: fun _ ->
           assert_equal "grass" (get_biome_for_level 1);
           assert_equal "grass" (get_biome_for_level 2);
           assert_equal "grass" (get_biome_for_level 3) );
         ( "test rock biome levels" >:: fun _ ->
           assert_equal "rock" (get_biome_for_level 4);
           assert_equal "rock" (get_biome_for_level 5);
           assert_equal "rock" (get_biome_for_level 6) );
         ( "test snow biome levels" >:: fun _ ->
           assert_equal "snow" (get_biome_for_level 7);
           assert_equal "snow" (get_biome_for_level 8);
           assert_equal "snow" (get_biome_for_level 9) );
         ( "test lava biome levels" >:: fun _ ->
           assert_equal "lava" (get_biome_for_level 10);
           assert_equal "lava" (get_biome_for_level 11);
           assert_equal "lava" (get_biome_for_level 12) );
       ]

let () =
  run_test_tt_main
    ("all_tests"
    >::: [
           obstacle_create_tests;
           obstacle_init_tests;
           death_message_tests;
           update_position_tests;
           biome_tests;
         ])
