type t = {
  x : float;
  y : float;
  width : float;
  height : float;
  name : string;
}

let min_spacing = ref 200.0
let max_spacing = ref 500.0
let ground_y = 410.0

type obstacle_dimensions = {
  min_height : float;
  max_height : float;
  min_width : float;
  max_width : float;
}

(* Getters for obstacle dimensions *)
(* let get_min_height obs = obs.min_height let get_max_height obs =
   obs.max_height let get_min_width obs = obs.min_width let get_max_width obs =
   obs.max_width *)

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
      x = Random.float (!max_spacing -. !min_spacing) +. x;
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

let init_obstacles biome total distance () =
  max_spacing := 200. +. distance;
  min_spacing := 500. +. distance;

  obstacle_tracking :=
    { spawned_count = 1; total_count = total; passed_count = 0 };
  [ create (1200.0 +. safe_start_distance) biome ]
