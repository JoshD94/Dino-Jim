open Raylib

type t = {
  x : float;
  y : float;
  width : float;
  height : float;
}

(* Constants *)
let min_height = 40.0
let max_height = 80.0
let obstacle_width = 20.0
let min_spacing = 300.0
let ground_y = 410.0

let safe_start_distance =
  400.0 (* Distance from start where first obstacle appears *)

(* Generate a new obstacle at given x position *)
let create x =
  let height = min_height +. Random.float (max_height -. min_height) in
  { x; y = ground_y -. height; width = obstacle_width; height }

(* Create initial set of obstacles *)
let init_obstacles () =
  Random.self_init ();
  [ create (1200.0 +. safe_start_distance) ]
(* Start first obstacle beyond safe distance *)

(* Update obstacle positions based on game speed *)
let update obstacles speed =
  (* Move obstacles left *)
  let moved = List.map (fun obs -> { obs with x = obs.x -. speed }) obstacles in

  (* Remove obstacles that are off screen *)
  let active = List.filter (fun obs -> obs.x +. obs.width > 0.0) moved in

  (* Add new obstacle if needed *)
  let rightmost =
    List.fold_left
      (fun max_x obs -> if obs.x > max_x then obs.x else max_x)
      0.0 active
  in

  if rightmost < 1200.0 -. min_spacing then create 1200.0 :: active else active

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

(* Render obstacles *)
let render obstacles =
  List.iter
    (fun obs ->
      draw_rectangle (int_of_float obs.x) (int_of_float obs.y)
        (int_of_float obs.width) (int_of_float obs.height) Color.darkgray)
    obstacles
