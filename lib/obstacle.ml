open Raylib
open ObstacleType

type t = obstacle

let min_spacing = 300.0
let max_spacing = 500.0
let ground_y = 410.0

type obstacle_dimensions = {
  min_height : float;
  max_height : float;
  min_width : float;
  max_width : float;
}

(* Obstacle dimensions *)
let cactus =
  { min_height = 40.0; max_height = 60.0; min_width = 10.0; max_width = 30.0 }

let rock =
  { min_height = 20.0; max_height = 40.0; min_width = 30.0; max_width = 50.0 }

let tree =
  { min_height = 60.0; max_height = 90.0; min_width = 10.0; max_width = 25.0 }

let bush =
  { min_height = 10.0; max_height = 30.0; min_width = 10.0; max_width = 30.0 }

let snowman =
  { min_height = 30.0; max_height = 50.0; min_width = 15.0; max_width = 25.0 }

let ice =
  { min_height = 10.0; max_height = 20.0; min_width = 20.0; max_width = 40.0 }

let evergreen =
  { min_height = 30.0; max_height = 40.0; min_width = 15.0; max_width = 20.0 }

let boulder =
  { min_height = 70.0; max_height = 80.0; min_width = 70.0; max_width = 80.0 }

let wall =
  { min_height = 60.0; max_height = 70.0; min_width = 20.0; max_width = 40.0 }

let lava =
  { min_height = 10.0; max_height = 20.0; min_width = 60.0; max_width = 100.0 }

let asteroid =
  { min_height = 30.0; max_height = 80.0; min_width = 30.0; max_width = 80.0 }

let fire =
  { min_height = 60.0; max_height = 80.0; min_width = 60.0; max_width = 80.0 }

let spike =
  { min_height = 30.0; max_height = 70.0; min_width = 20.0; max_width = 40.0 }

(* Obstacle types *)
let grass =
  [| (cactus, "cactus"); (rock, "rock"); (tree, "tree"); (bush, "bush") |]

let snow =
  [|
    (snowman, "snowman"); (ice, "ice"); (rock, "rock"); (evergreen, "evergreen");
  |]

let rock =
  [|
    (rock, "rock");
    (boulder, "boulder");
    (wall, "wall");
    (evergreen, "evergreen");
  |]

let lava =
  [| (lava, "lava"); (asteroid, "asteroid"); (fire, "fire"); (spike, "spike") |]

(* Safe start distance *)
let safe_start_distance =
  400.0 (* Distance from start where first obstacle appears *)

(* Generate a new obstacle at given x position *)
let create x biome =
  Random.self_init ();
  let random_num = Random.int 4 in
  if biome = "grass" then
    let obs = fst grass.(random_num) in
    let height =
      obs.min_height +. Random.float (obs.max_height -. obs.min_height)
    in
    let width =
      obs.min_width +. Random.float (obs.max_width -. obs.min_width)
    in
    {
      x = Random.float (max_spacing -. min_spacing) +. x;
      y = ground_y -. height;
      width;
      height;
      name = snd grass.(random_num);
    }
  else if biome = "snow" then
    let obs = fst snow.(random_num) in
    let height =
      obs.min_height +. Random.float (obs.max_height -. obs.min_height)
    in
    let width =
      obs.min_width +. Random.float (obs.max_width -. obs.min_width)
    in
    { x; y = ground_y -. height; width; height; name = snd snow.(random_num) }
  else if biome = "rock" then
    let obs = fst rock.(random_num) in
    let height =
      obs.min_height +. Random.float (obs.max_height -. obs.min_height)
    in
    let width =
      obs.min_width +. Random.float (obs.max_width -. obs.min_width)
    in
    { x; y = ground_y -. height; width; height; name = snd rock.(random_num) }
  else if biome = "lava" then
    let obs = fst lava.(random_num) in
    let height =
      obs.min_height +. Random.float (obs.max_height -. obs.min_height)
    in
    let width =
      obs.min_width +. Random.float (obs.max_width -. obs.min_width)
    in
    { x; y = ground_y -. height; width; height; name = snd lava.(random_num) }
  else
    let obs = fst grass.(random_num) in
    let height =
      obs.min_height +. Random.float (obs.max_height -. obs.min_height)
    in
    let width =
      obs.min_width +. Random.float (obs.max_width -. obs.min_width)
    in
    { x; y = ground_y -. height; width; height; name = snd grass.(random_num) }

type obstacle_state = {
  spawned_count : int;
  total_count : int;
  passed_count : int;
}

let obstacle_tracking =
  ref { spawned_count = 1; total_count = 0; passed_count = 0 }

let get_passed_count () = !obstacle_tracking.passed_count

let init_obstacles biome total () =
  obstacle_tracking :=
    { spawned_count = 1; total_count = total; passed_count = 0 };
  [ create (1200.0 +. safe_start_distance) biome ]

let update obstacles speed biome =
  (* Move obstacles left *)
  let moved = List.map (fun obs -> { obs with x = obs.x -. speed }) obstacles in

  (* Count obstacles that went off screen *)
  let active = List.filter (fun obs -> obs.x +. obs.width > 0.0) moved in
  let passed = List.length moved - List.length active in

  (* Update passed count in our state *)
  obstacle_tracking :=
    {
      !obstacle_tracking with
      passed_count = !obstacle_tracking.passed_count + passed;
    };

  (* Add new obstacle if needed and we haven't spawned all obstacles yet *)
  let rightmost =
    List.fold_left
      (fun max_x obs -> if obs.x > max_x then obs.x else max_x)
      0.0 active
  in

  if
    rightmost < 1200.0 -. min_spacing
    && !obstacle_tracking.spawned_count < !obstacle_tracking.total_count
  then (
    obstacle_tracking :=
      {
        !obstacle_tracking with
        spawned_count = !obstacle_tracking.spawned_count + 1;
      };
    create 1200.0 biome :: active)
  else active

(* Check collision between player and obstacles *)
let check_collision player_x player_y player_width player_height obstacles =
  let player_rect =
    Rectangle.create (player_x +. 5.0) (player_y +. 5.0) (player_width -. 10.0)
      (player_height -. 10.0)
  in

  List.exists
    (fun obs ->
      let obstacle_rect = Rectangle.create obs.x obs.y obs.width obs.height in
      check_collision_recs player_rect obstacle_rect)
    obstacles

let draw_obstacle (obs : obstacle) =
  match obs.name with
  | "cactus" ->
      (* Main body *)
      draw_rectangle (int_of_float obs.x) (int_of_float obs.y)
        (int_of_float obs.width) (int_of_float obs.height)
        (Color.create 0 128 17 255);
      (* Right arm - horizontal part *)
      draw_rectangle
        (int_of_float (obs.x +. obs.width))
        (int_of_float (obs.y +. (obs.height /. 2.0)))
        (int_of_float (obs.width /. 2.0))
        (int_of_float (obs.width /. 2.0))
        (Color.create 0 128 17 255);
      (* Right arm - vertical part *)
      draw_rectangle
        (int_of_float (obs.x +. obs.width +. (obs.width /. 4.0)))
        (int_of_float (obs.y +. (obs.height /. 4.)))
        (int_of_float (obs.width /. 2.0))
        (int_of_float (obs.height /. 2.))
        (Color.create 0 128 17 255)
  | "rock" ->
      (* Main rock body *)
      draw_rectangle (int_of_float obs.x) (int_of_float obs.y)
        (int_of_float obs.width) (int_of_float obs.height)
        (Color.create 128 128 128 255);
      (* Color detail *)
      draw_rectangle
        (int_of_float (obs.x +. (obs.width /. 4.)))
        (int_of_float (obs.y +. (obs.height /. 3.)))
        (int_of_float (obs.width /. 3.))
        (int_of_float (obs.height /. 3.))
        (Color.create 90 90 90 255);
      (* Color detail *)
      draw_rectangle
        (int_of_float (obs.x +. (obs.width /. 2.)))
        (int_of_float (obs.y +. (obs.height /. 2.)))
        (int_of_float (obs.width /. 2.))
        (int_of_float (obs.height /. 3.))
        (Color.create 90 90 90 255)
  | "tree" ->
      (* Trunk *)
      draw_rectangle
        (int_of_float (obs.x +. (obs.width /. 4.)))
        (int_of_float (obs.y +. (obs.height /. 3.)))
        (int_of_float (obs.width /. 2.))
        (int_of_float (obs.height *. 2. /. 3.))
        (Color.create 139 69 19 255);
      (* Leaves *)
      draw_rectangle
        (int_of_float (obs.x -. (obs.width /. 2.)))
        (int_of_float obs.y)
        (int_of_float (obs.width *. 2.))
        (int_of_float (obs.height /. 2.))
        (Color.create 34 139 34 255)
  | "bush" ->
      (* Main bush *)
      draw_rectangle (int_of_float obs.x) (int_of_float obs.y)
        (int_of_float obs.width) (int_of_float obs.height)
        (Color.create 34 139 34 255);
      (* Bush detail *)
      draw_rectangle
        (int_of_float (obs.x +. (obs.width /. 2.)))
        (int_of_float (obs.y -. (obs.height *. 1.6) +. obs.height))
        (int_of_float (obs.width *. 2.4))
        (int_of_float (obs.height *. 1.6))
        (Color.create 34 139 34 255)
  | "snowman" ->
      (* Bottom sphere *)
      draw_rectangle (int_of_float obs.x)
        (int_of_float (obs.y +. (obs.height *. 0.6)))
        (int_of_float obs.width)
        (int_of_float (obs.height *. 0.4))
        (Color.create 105 217 255 255);
      (* Middle sphere *)
      draw_rectangle
        (int_of_float (obs.x +. (obs.width *. 0.1)))
        (int_of_float (obs.y +. (obs.height *. 0.3)))
        (int_of_float (obs.width *. 0.8))
        (int_of_float (obs.height *. 0.3))
        (Color.create 105 217 255 255);
      (* Head *)
      draw_rectangle
        (int_of_float (obs.x +. (obs.width *. 0.2)))
        (int_of_float obs.y)
        (int_of_float (obs.width *. 0.6))
        (int_of_float (obs.height *. 0.3))
        (Color.create 105 217 255 255)
      (* Border *)
  | "ice" ->
      (* Main ice block *)
      draw_rectangle (int_of_float obs.x) (int_of_float obs.y)
        (int_of_float obs.width) (int_of_float obs.height)
        (Color.create 173 216 230 150);
      (* Shine effect *)
      draw_rectangle
        (int_of_float (obs.x +. (obs.width *. 0.2)))
        (int_of_float (obs.y +. (obs.height *. 0.2)))
        (int_of_float (obs.width *. 0.2))
        (int_of_float (obs.height *. 0.2))
        (Color.create 255 255 255 200)
  | "evergreen" ->
      (* Main tree body *)
      draw_rectangle (int_of_float obs.x) (int_of_float obs.y)
        (int_of_float obs.width) (int_of_float obs.height)
        (Color.create 139 69 19 255);
      (* Tree top *)
      draw_triangle
        (Vector2.create
           (obs.x +. (obs.width /. 2.))
           (obs.y -. (obs.height *. 2.)))
        (Vector2.create
           (obs.x -. (obs.width /. 2.))
           (obs.y +. (obs.height *. 0.3)))
        (Vector2.create
           (obs.x +. obs.width +. (obs.width /. 2.))
           (obs.y +. (obs.height *. 0.3)))
        (Color.create 34 139 34 255)
  | "boulder" ->
      (* Main boulder body *)
      draw_rectangle (int_of_float obs.x) (int_of_float obs.y)
        (int_of_float obs.width) (int_of_float obs.height)
        (Color.create 128 128 128 255);
      (* Boulder details *)
      draw_circle
        (int_of_float (obs.x +. (obs.width *. 0.3)))
        (int_of_float (obs.y +. (obs.height *. 0.3)))
        (float_of_int 5)
        (Color.create 90 90 90 255);
      draw_circle
        (int_of_float (obs.x +. (obs.width *. 0.7)))
        (int_of_float (obs.y +. (obs.height *. 0.6)))
        (float_of_int 3)
        (Color.create 90 90 90 255)
  | "wall" ->
      (* Main wall *)
      draw_rectangle (int_of_float obs.x) (int_of_float obs.y)
        (int_of_float obs.width) (int_of_float obs.height)
        (Color.create 87 10 23 255);
      (* Brick pattern *)
      for i = 0 to 2 do
        draw_line (int_of_float obs.x)
          (int_of_float (obs.y +. (float_of_int i *. obs.height /. 3.)))
          (int_of_float (obs.x +. obs.width))
          (int_of_float (obs.y +. (float_of_int i *. obs.height /. 3.)))
          (Color.create 0 0 0 255)
      done
  | "lava" ->
      (* Lava pool *)
      draw_rectangle (int_of_float obs.x) (int_of_float obs.y)
        (int_of_float obs.width) (int_of_float obs.height)
        (Color.create 255 69 0 255);
      (* Lava bubbles *)
      draw_circle
        (int_of_float (obs.x +. (obs.width *. 0.3)))
        (int_of_float (obs.y +. (obs.height *. 0.5)))
        (float_of_int 3)
        (Color.create 255 165 0 255)
  | "asteroid" ->
      (* Main asteroid body *)
      draw_rectangle (int_of_float obs.x) (int_of_float obs.y)
        (int_of_float obs.width) (int_of_float obs.height)
        (Color.create 128 128 128 255);
      (* Crater details *)
      draw_circle
        (int_of_float (obs.x +. (obs.width *. 0.3)))
        (int_of_float (obs.y +. (obs.height *. 0.3)))
        (float_of_int 4)
        (Color.create 90 90 90 255);
      draw_circle
        (int_of_float (obs.x +. (obs.width *. 0.7)))
        (int_of_float (obs.y +. (obs.height *. 0.6)))
        (float_of_int 3)
        (Color.create 90 90 90 255)
  | "fire" ->
      for _ = 0 to 7 do
        let x = Random.float obs.width in
        let y = Random.float obs.height in
        let r = 3 + Random.int 3 in
        draw_circle
          (int_of_float (obs.x +. x))
          (int_of_float (obs.y +. y))
          (float_of_int r)
          (Color.create 255 69 0 255);
        (* Draw base *)
        draw_rectangle (int_of_float obs.x)
          (int_of_float (obs.y +. obs.height))
          (int_of_float obs.width) (int_of_float 2.) (Color.create 92 13 5 255)
      done
  | "spike" ->
      (* Main spike body *)
      draw_triangle
        (Vector2.create (obs.x +. (obs.width /. 2.)) (obs.y -. obs.height))
        (Vector2.create obs.x (obs.y +. obs.height))
        (Vector2.create (obs.x +. obs.width) (obs.y +. obs.height))
        (Color.create 90 90 90 255);
      (* Second spike *)
      draw_triangle
        (Vector2.create
           (obs.x +. (obs.width *. 1.) +. (obs.width /. 2.))
           (obs.y -. (obs.height /. 2.5)))
        (Vector2.create
           (obs.x +. (obs.width *. 1.) +. (obs.width /. 4.))
           (obs.y +. obs.height))
        (Vector2.create
           (obs.x +. (obs.width *. 1.) +. (obs.width *. 0.75))
           (obs.y +. obs.height))
        (Color.create 90 90 90 255)
  | _ ->
      (* Default fallback drawing *)
      draw_rectangle (int_of_float obs.x) (int_of_float obs.y)
        (int_of_float obs.width) (int_of_float obs.height) Color.black

(* Render obstacles *)
let render obstacles = List.iter (fun obs -> draw_obstacle obs) obstacles
