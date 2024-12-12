open Jim.Skins
open Raylib
open Jim.Player
open Jim.Game
open Jim.Obstacle
open Jim.Chest

module Background = struct
  let grass_rgb = (34, 139, 34) (* Green *)
  let snow_rgb = (135, 206, 235) (* Light Blue *)
  let rock_rgb = (128, 128, 128) (* Gray *)
  let lava_rgb = (178, 34, 34) (* Red *)

  (* Helper for linear interpolation of a single number *)
  let lerp a b t =
    let t = max 0.0 (min 1.0 t) in
    (a *. (1.0 -. t)) +. (b *. t)

  (* Get biome based on level number *)
  let get_biome_for_level level =
    if level <= 3 then grass_rgb
    else if level <= 6 then snow_rgb
    else if level <= 9 then rock_rgb
    else lava_rgb

  (* Draw the background with gradual color transitions *)
  let draw scroll_y screen_width screen_height =
    let level_height = float_of_int screen_height *. 0.333 in

    (* Calculate current level and transition progress *)
    let level_position = scroll_y /. level_height in
    let current_level = int_of_float level_position + 1 in
    let next_level = current_level + 1 in

    (* Get RGB values for current and next levels *)
    let current_rgb = get_biome_for_level current_level in
    let next_rgb = get_biome_for_level next_level in

    (* Calculate transition progress within the level *)
    let transition_progress =
      level_position -. float_of_int (current_level - 1)
    in

    (* Get RGB components *)
    let r1, g1, b1 = current_rgb in
    let r2, g2, b2 = next_rgb in

    (* Interpolate RGB components *)
    let r =
      int_of_float
        (lerp (float_of_int r1) (float_of_int r2) transition_progress)
    in
    let g =
      int_of_float
        (lerp (float_of_int g1) (float_of_int g2) transition_progress)
    in
    let b =
      int_of_float
        (lerp (float_of_int b1) (float_of_int b2) transition_progress)
    in

    (* Draw the interpolated color *)
    draw_rectangle 0 0 screen_width screen_height (Color.create r g b 255)
end

(* Obstacle module *)

type obstacle = Jim.Obstacle.t

let get_passed_count () = !obstacle_tracking.passed_count

let update (obstacles : obstacle list) speed biome =
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
    rightmost < 1200.0 -. !min_spacing
    && !obstacle_tracking.spawned_count < !obstacle_tracking.total_count
  then (
    obstacle_tracking :=
      {
        !obstacle_tracking with
        spawned_count = !obstacle_tracking.spawned_count + 1;
      };
    create 1200.0 biome :: active)
  else active

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

(* Game module *)

(* Game constants *)
let initial_speed = 5.0

let check_back_button () =
  if is_mouse_button_pressed MouseButton.Left then
    let mouse_pos = get_mouse_position () in
    let mouse_x = Vector2.x mouse_pos in
    let mouse_y = Vector2.y mouse_pos in
    mouse_x >= 20.0 && mouse_x <= 120.0 && mouse_y >= 20.0 && mouse_y <= 60.0
  else false

let check_retry_button () =
  if is_mouse_button_pressed MouseButton.Left then
    let mouse_pos = get_mouse_position () in
    let mouse_x = Vector2.x mouse_pos in
    let mouse_y = Vector2.y mouse_pos in
    mouse_x >= 500.0 && mouse_x <= 700.0 && mouse_y >= 300.0 && mouse_y <= 360.0
  else false

let render_completion () =
  (* completion overlay *)
  draw_rectangle 0 0 1200 800 (Color.create 0 0 0 150);

  (* completion message *)
  let message = "Level Complete!" in
  let font_size = 60 in
  let text_width = measure_text message font_size in
  draw_text message ((1200 - text_width) / 2) 200 font_size Color.white;

  (* retry button *)
  draw_rectangle 500 300 200 60 Color.green;
  let button_text = "Back to Menu" in
  let btn_font_size = 20 in
  let btn_text_width = measure_text button_text btn_font_size in
  draw_text button_text
    (600 - (btn_text_width / 2))
    320 btn_font_size Color.white

let render_death state =
  (* overlay *)
  draw_rectangle 0 0 1200 800 (Color.create 0 0 0 150);

  (* death message *)
  let font_size = 60 in
  let text_width = measure_text state.death_message font_size in
  draw_text state.death_message
    ((1200 - text_width) / 2)
    200 font_size Color.black;

  (* retry button *)
  draw_rectangle 500 300 200 60 Color.red;
  let button_text = "Back to Menu" in
  let btn_font_size = 20 in
  let btn_text_width = measure_text button_text btn_font_size in
  draw_text button_text
    (600 - (btn_text_width / 2))
    320 btn_font_size Color.white

let render state player =
  begin_drawing ();
  clear_background Color.raywhite;

  (* Ground line *)
  draw_line 0 410 1200 410 Color.black;

  (* obstacles *)
  render state.obstacles;

  let pos = state.pos in
  (* current skin *)
  ignore ((current_skin player) pos.x pos.y);

  (* progress *)
  let progress_text =
    Printf.sprintf "Obstacles: %d/%d" state.obstacles_passed
      state.total_obstacles
  in
  draw_text progress_text 20 70 20 Color.black;

  (* back button *)
  draw_rectangle 20 20 100 40 Color.gray;
  draw_text "Back" 45 30 20 Color.white;

  (* draw completion screen if level is complete *)
  if state.completed then render_completion ()
  else if state.died then render_death state;

  end_drawing ()

(* Helper function to check which obstacle was hit *)
let get_collision_obstacle player_x player_y player_width player_height
    obstacles =
  let player_rect =
    Rectangle.create (player_x +. 5.0) (player_y +. 5.0) (player_width -. 10.0)
      (player_height -. 10.0)
  in

  let rec check_obstacles = function
    | [] -> None
    | obs :: rest ->
        let obstacle_rect = Rectangle.create obs.x obs.y obs.width obs.height in
        if check_collision_recs player_rect obstacle_rect then Some obs.name
        else check_obstacles rest
  in
  check_obstacles obstacles

(* main game initialization and loop *)
let init_game lvl obstacle_count speed_mul gravity jump_force biome player () =
  print_endline
    (string_of_int lvl ^ " "
    ^ string_of_int obstacle_count
    ^ " " ^ string_of_int speed_mul ^ " " ^ string_of_float gravity ^ " "
    ^ string_of_float jump_force);

  (* set distance between obstacles based on level *)
  let distance = float_of_int speed_mul *. 60. in

  (* initial game state *)
  let initial_state =
    {
      pos = { x = 100.0; y = 350.0; velocity_y = 0.0 };
      obstacles = init_obstacles biome obstacle_count distance ();
      speed = initial_speed *. float_of_int speed_mul;
      obstacles_passed = 0;
      total_obstacles = obstacle_count;
      completed = false;
      died = false;
      death_message = "";
    }
  in

  (* main game loop *)
  let rec game_loop state player =
    if window_should_close () then ()
    else if check_back_button () then ()
    else if (state.completed || state.died) && check_retry_button () then (
      if state.completed then complete_level player lvl;
      if state.died then () else add_coins player (get_level_reward lvl);
      ())
    else
      (* Update game state *)
      let new_pos = update_position state.pos gravity jump_force in
      let new_obstacles = update state.obstacles state.speed biome in

      (* get current passed count *)
      let obstacles_passed = get_passed_count () in

      (* check completion *)
      let completed = obstacles_passed >= state.total_obstacles in

      (* check for collision and get obstacle name *)
      match
        get_collision_obstacle new_pos.x new_pos.y 30.0 60.0 new_obstacles
      with
      | Some obstacle_name ->
          (* Update state with death message based on obstacle hit *)
          let death_state =
            {
              state with
              died = true;
              death_message = get_death_message obstacle_name;
            }
          in
          render death_state player;
          game_loop death_state player
      | None ->
          (* Update state and continue *)
          let new_state =
            {
              pos = new_pos;
              obstacles = new_obstacles;
              speed = state.speed;
              obstacles_passed;
              total_obstacles = state.total_obstacles;
              completed;
              died = false;
              death_message = "";
            }
          in

          render new_state player;
          game_loop new_state player
  in

  (* start game loop *)
  game_loop initial_state player

(* Skin select module *)
let button_height = 40.0
let button_width = 160.0
let padding = 120.0
let grid_cols = 3
let num_items = 12

let skin_list =
  [|
    DefaultSkin.draw;
    SantaJim.draw;
    AngryJim.draw;
    GreenJim.draw;
    RedJim.draw;
    BlueJim.draw;
    InvisibleJim.draw;
    OrangeJim.draw;
    DarthJim.draw;
    MagentaJim.draw;
    YellowJim.draw;
    PurpleJim.draw;
  |]

let skin_names =
  [|
    "Default Jim";
    "Santa Jim";
    "Angry Jim";
    "Green Jim";
    "Red Jim";
    "Blue Jim";
    "Invisible Jim";
    "Orange Jim";
    "Darth Jim";
    "Magenta Jim";
    "Yellow Jim";
    "Purple Jim";
  |]

let check_back_button () =
  if is_mouse_button_pressed MouseButton.Left then
    let mouse_pos = get_mouse_position () in
    let mouse_x = Vector2.x mouse_pos in
    let mouse_y = Vector2.y mouse_pos in
    mouse_x >= 20.0 && mouse_x <= 120.0 && mouse_y >= 20.0 && mouse_y <= 60.0
  else false

(* Run function to open the skin select menu *)
let run_skin_select player =
  let screen_width = 1200 in
  let rec game_loop state =
    if window_should_close () then ()
    else if check_back_button () then ()
    else begin
      begin_drawing ();
      clear_background Color.raywhite;

      (* Draw title *)
      let title = "Skin Select" in
      let title_size = 40 in
      let title_width = measure_text title title_size in
      draw_text title
        ((screen_width - title_width) / 2)
        40 title_size Color.black;

      (* Draw select buttons *)
      for i = 0 to num_items - 1 do
        let has = has_skin state skin_list.(i) in
        let row = float_of_int (i / grid_cols) in
        let col = float_of_int (i mod grid_cols) in
        let x =
          (1200.0 -. (float_of_int grid_cols *. (button_width +. padding)))
          /. 2.0
          +. (col *. (button_width +. padding))
        in
        let y = 190.0 +. (row *. (button_height +. padding)) in

        (* Draw button *)
        let color =
          if
            has
            && current_skin state 10000. 10000. = skin_list.(i) 10000. 10000.
          then Color.maroon
          else Color.red
        in
        draw_rectangle (int_of_float x) (int_of_float y)
          (int_of_float button_width)
          (int_of_float button_height)
          color;
        (* Draw Skins *)
        ignore (skin_list.(i) (x +. 80.) (y -. 70.));
        (* Draw back button last so it's always on top *)
        draw_rectangle 20 20 100 40 Color.gray;
        draw_text "Back" 45 30 20 Color.white;
        (* Draw Header *)
        let head = skin_names.(i) in
        let text_size = 15 in
        let text_width = measure_text head text_size in
        let text_x = x +. ((button_width -. float_of_int text_width) /. 2.0) in
        let text_y =
          y -. 120. +. ((button_height -. float_of_int text_size) /. 2.0)
        in
        draw_text head (int_of_float text_x) (int_of_float text_y) text_size
          Color.black;
        (* Draw Label *)
        let str = if has then "Select" else "Not Owned" in
        let text_size = 15 in
        let text_width = measure_text str text_size in
        let text_x = x +. ((button_width -. float_of_int text_width) /. 2.0) in
        let text_y = y +. ((button_height -. float_of_int text_size) /. 2.0) in
        draw_text str (int_of_float text_x) (int_of_float text_y) text_size
          Color.black;

        (* Check for button click *)
        if is_mouse_button_pressed MouseButton.Left then begin
          let mouse_pos = get_mouse_position () in
          let mouse_x = Vector2.x mouse_pos in
          let mouse_y = Vector2.y mouse_pos in
          if
            mouse_x >= x
            && mouse_x <= x +. button_width
            && mouse_y >= y
            && mouse_y <= y +. button_height
            && has_skin state skin_list.(i)
          then select_skin state skin_list.(i)
        end
      done;

      end_drawing ();
      game_loop state
    end
  in
  game_loop player

(* Shop module *)

let button_height = 80.0
let button_width = 160.0
let padding = 120.0
let grid_cols = 4
let num_items = 7
let chest = create_chest ()

let buyable_items =
  [|
    ("Red Jim", 10);
    ("Green Jim", 10);
    ("Blue Jim", 10);
    ("Orange Jim", 10);
    ("Purple Jim", 15);
    ("Yellow Jim", 15);
    ("Chest", 30);
  |]

let buyable_skins =
  [|
    RedJim.draw;
    GreenJim.draw;
    BlueJim.draw;
    OrangeJim.draw;
    PurpleJim.draw;
    YellowJim.draw;
  |]

let bought_items = [| false; false; false; false; false; false; false |]

(*let rec check_item lst item = match lst with | [] -> false | h :: t -> if item
  = h then true else check_item t item

  let buy_item state (item, cost) = if check_item state.bought_items item then
  state else if state.coins >= cost then { coins = state.coins - cost;
  bought_items = item :: state.bought_items } else state *)

let check_back_button () =
  if is_mouse_button_pressed MouseButton.Left then
    let mouse_pos = get_mouse_position () in
    let mouse_x = Vector2.x mouse_pos in
    let mouse_y = Vector2.y mouse_pos in
    mouse_x >= 20.0 && mouse_x <= 120.0 && mouse_y >= 20.0 && mouse_y <= 60.0
  else false

(* Run function to open the shop *)
let run_shop player =
  let screen_width = 1200 in
  let rec game_loop state =
    if window_should_close () then ()
    else if check_back_button () then ()
    else begin
      begin_drawing ();
      clear_background Color.raywhite;

      (* Draw title *)
      let title = "Shop" in
      let title_size = 40 in
      let title_width = measure_text title title_size in
      draw_text title
        ((screen_width - title_width) / 2)
        40 title_size Color.black;

      (* Draw shop buttons *)
      for i = 0 to num_items - 1 do
        if i < num_items - 1 && has_skin player buyable_skins.(i) then
          bought_items.(i) <- true;
        let row = float_of_int (i / grid_cols) in
        let col = float_of_int (i mod grid_cols) in
        let x =
          (1200.0 -. (float_of_int grid_cols *. (button_width +. padding)))
          /. 2.0
          +. (col *. (button_width +. padding))
        in
        let y = 160.0 +. (row *. (button_height +. padding)) in

        (* Draw button *)
        let color =
          if i = num_items - 1 && more_skins chest then Color.red
          else if bought_items.(i) then Color.maroon
          else Color.red
        in
        draw_rectangle (int_of_float x) (int_of_float y)
          (int_of_float button_width)
          (int_of_float button_height)
          color;
        (* Draw Skins *)
        if i < num_items - 1 then
          ignore (buyable_skins.(i) (x +. 75.) (y -. 65.));
        (* Draw back button last so it's always on top *)
        draw_rectangle 20 20 100 40 Color.gray;
        draw_text "Back" 45 30 20 Color.white;

        (* Draw Label *)
        let str =
          fst buyable_items.(i)
          ^ ": "
          ^ string_of_int (snd buyable_items.(i))
          ^ " coins"
        in
        let text_size = 15 in
        let text_width = measure_text str text_size in
        let text_x = x +. ((button_width -. float_of_int text_width) /. 2.0) in
        let text_y = y +. ((button_height -. float_of_int text_size) /. 2.0) in
        draw_text str (int_of_float text_x) (int_of_float text_y) text_size
          Color.black;
        let coin_amount_str = "Coins: " ^ string_of_int (coins state) in
        draw_text coin_amount_str
          (screen_width - measure_text coin_amount_str 25 - 10)
          20 25 Color.black;

        (* Check for button click *)
        if is_mouse_button_pressed MouseButton.Left then begin
          let mouse_pos = get_mouse_position () in
          let mouse_x = Vector2.x mouse_pos in
          let mouse_y = Vector2.y mouse_pos in
          if
            mouse_x >= x
            && mouse_x <= x +. button_width
            && mouse_y >= y
            && mouse_y <= y +. button_height
            && bought_items.(i) = false
            && coins state >= snd buyable_items.(i)
          then (
            add_coins state (0 - snd buyable_items.(i));
            if i < num_items - 1 then (
              add_skin player buyable_skins.(i);
              bought_items.(i) <- true;
              save_state player);
            if i = num_items - 1 then (
              open_chest chest player;
              save_state player;
              if more_skins chest <> true then bought_items.(i) <- true))
        end
      done;

      end_drawing ();
      game_loop player
    end
  in
  game_loop player

(* Menu module *)

type game_state =
  | Onboarding
  | LevelSelect
  | Level of int
  | Shop
  | SkinSelect

type scroll_state = {
  mutable scroll_y : float;
  scroll_speed : float;
  max_scroll : float;
}

let button_size = 100.0
let num_levels = 12
let view_height = 800.0
let level_spacing = 266.7
let total_height = float_of_int num_levels *. level_spacing
let screen_width = 1200
let screen_height = 800
let top_padding = 90.0
let calculate_obstacle_count level = 10 + ((level - 1) * 3)

let get_path_points scroll_y =
  Array.init num_levels (fun i ->
      let base_y = float_of_int i *. level_spacing in
      let adjusted_y = base_y -. scroll_y +. top_padding in
      if i mod 2 = 0 then (400.0, adjusted_y +. 100.0)
      else (800.0, adjusted_y +. 100.0))

let draw_path points =
  for i = 0 to Array.length points - 2 do
    let x1, y1 = points.(i) in
    let x2, y2 = points.(i + 1) in
    draw_line_ex (Vector2.create x1 y1) (Vector2.create x2 y2) 5.0
      (Color.create 200 200 200 255)
  done

let draw_header () =
  let shop_width = 80 in
  let shop_height = 40 in
  let shop_x = screen_width - shop_width - 20 in
  let shop_y = 20 in
  draw_rectangle shop_x shop_y shop_width shop_height Color.gray;
  draw_text "Shop" (shop_x + 20) (shop_y + 10) 20 Color.white;
  (shop_x, shop_y, shop_width, shop_height)

let draw_skin_select_header () =
  let select_width = 160 in
  let select_height = 40 in
  let select_x = 20 in
  let select_y = 20 in
  draw_rectangle select_x select_y select_width select_height Color.gray;
  draw_text "Skin select" (select_x + 20) (select_y + 10) 20 Color.white;
  (select_x, select_y, select_width, select_height)

let draw_title () =
  let title = "Level Select" in
  let title_size = 40 in
  let title_width = measure_text title title_size in

  let padding = 20 in
  let bg_rect_width = title_width + (padding * 2) in
  let bg_rect_height = title_size + padding in
  let bg_rect_x = (screen_width - bg_rect_width) / 2 in
  let bg_rect_y = 40 - (padding / 2) in

  draw_rectangle bg_rect_x bg_rect_y bg_rect_width bg_rect_height Color.raywhite;
  draw_text title ((screen_width - title_width) / 2) 40 title_size Color.black

let draw_level_bubble player level_num (x, y) =
  let radius = button_size /. 2.0 in

  let unlocked = is_level_unlocked player level_num in

  (* Draw shadow *)
  draw_circle
    (int_of_float (x +. 2.0))
    (int_of_float (y +. 2.0))
    radius (Color.create 0 0 0 40);

  (* Draw main circle with biome colors *)
  let color =
    if not unlocked then Color.black
    else
      match get_biome_for_level level_num with
      | "grass" -> Color.create 24 97 24 255 (* Forest Green *)
      | "rock" -> Color.create 89 89 89 255 (* Rock Gray *)
      | "snow" -> Color.create 94 144 164 255 (* Ice Blue *)
      | "lava" -> Color.create 124 24 24 255 (* Lava Red *)
      | _ -> Color.lightgray
  in
  draw_circle (int_of_float x) (int_of_float y) radius color;

  (* Draw level number *)
  let number_str = string_of_int level_num in
  let text_size = 30 in
  let text_width = measure_text number_str text_size in
  let text_x = x -. (float_of_int text_width /. 2.0) in
  let text_y = y -. (float_of_int text_size /. 2.0) in
  let text_color = if unlocked then Color.white else Color.gray in
  draw_text number_str (int_of_float text_x) (int_of_float text_y) text_size
    text_color;

  (* Draw lock symbol if level is locked *)
  if not unlocked then begin
    (* Draw substantial lock with bridge top *)
    let base_width = 45.0 in

    (* Wider base *)
    let bridge_width = 35.0 in

    (* Narrower bridge *)
    let lock_y = y -. (base_width /. 2.0) +. 15.0 in

    (* Draw wider base rectangle (body of lock) *)
    let base_x = x -. (base_width /. 2.0) in

    draw_rectangle (int_of_float base_x) (int_of_float lock_y)
      (int_of_float base_width)
      (int_of_float (base_width *. 0.8))
      Color.white;

    (* Draw bridge top - positioned relative to base *)
    let bridge_height = base_width *. 0.6 in

    let pillar_width = bridge_width *. 0.2 in

    let bridge_x = x -. (bridge_width /. 2.0) in

    (* Left pillar *)
    draw_rectangle (int_of_float bridge_x)
      (int_of_float (lock_y -. bridge_height))
      (int_of_float pillar_width)
      (int_of_float bridge_height)
      Color.white;

    (* Right pillar *)
    draw_rectangle
      (int_of_float (bridge_x +. bridge_width -. pillar_width))
      (int_of_float (lock_y -. bridge_height))
      (int_of_float pillar_width)
      (int_of_float bridge_height)
      Color.white;

    (* Top connecting piece *)
    draw_rectangle (int_of_float bridge_x)
      (int_of_float (lock_y -. bridge_height))
      (int_of_float bridge_width)
      (int_of_float (bridge_height *. 0.3))
      Color.white
  end

let check_level_click mouse_pos (x, y) =
  let radius = button_size /. 2.0 in
  let dx = Vector2.x mouse_pos -. x in
  let dy = Vector2.y mouse_pos -. y in
  (dx *. dx) +. (dy *. dy) <= radius *. radius

let update_scroll scroll_state =
  let wheel_move = get_mouse_wheel_move () in

  let scroll_delta =
    if wheel_move != 0.0 then wheel_move *. scroll_state.scroll_speed *. -1.0
    else
      let gesture_event = get_gesture_drag_vector () in
      Vector2.y gesture_event *. 2.0
  in

  scroll_state.scroll_y <- scroll_state.scroll_y +. scroll_delta;

  if is_key_down Key.Down then
    scroll_state.scroll_y <-
      scroll_state.scroll_y +. (get_frame_time () *. 300.0)
  else if is_key_down Key.Up then
    scroll_state.scroll_y <-
      scroll_state.scroll_y -. (get_frame_time () *. 300.0);

  scroll_state.scroll_y <-
    max 0.0 (min scroll_state.scroll_y scroll_state.max_scroll)

let word_wrap text max_width font_size =
  let words = String.split_on_char ' ' text in
  let rec wrap_lines acc current_line current_width = function
    | [] -> List.rev (current_line :: acc)
    | word :: rest ->
        let word_width = measure_text (word ^ " ") font_size in
        if current_width + word_width <= max_width then
          wrap_lines acc
            (current_line ^ (if current_line = "" then "" else " ") ^ word)
            (current_width + word_width)
            rest
        else wrap_lines (current_line :: acc) word word_width rest
  in
  wrap_lines [] "" 0 words

let draw_onboarding () =
  clear_background Color.raywhite;

  (* Draw title *)
  let title = "The Story of Jim" in
  let title_size = 40 in
  let title_width = measure_text title title_size in
  draw_text title ((screen_width - title_width) / 2) 100 title_size Color.black;

  (* Draw story text *)
  let story =
    "Deep in the heart of the prehistoric world, Jim the dinosaur lived \
     peacefully with his family. But when news spread of an incoming asteroid \
     threatening their world, Jim found himself separated from his loved ones. \
     Now he must race against time, crossing through grasslands, rocky \
     mountains, snowy peaks, and dangerous lava fields to reunite with his \
     family before it's too late. Help Jim overcome obstacles and navigate \
     through treacherous terrains in his desperate journey home!"
  in

  (* Draw wrapped story text *)
  let font_size = 25 in
  let max_width = 800 in
  let y = 200 in
  let line_height = font_size + 10 in

  (* Add some spacing between lines *)
  let wrapped_lines = word_wrap story max_width font_size in
  List.iteri
    (fun i line ->
      let line_width = measure_text line font_size in
      let line_x = (screen_width - line_width) / 2 in
      (* Center each line *)
      let line_y = y + (i * line_height) in
      draw_text line line_x line_y font_size Color.black)
    wrapped_lines;

  (* Draw continue button *)
  let button_width = 200 in
  let button_height = 60 in
  let button_x = (screen_width - button_width) / 2 in
  let button_y = 600 in

  draw_rectangle button_x button_y button_width button_height Color.green;
  let button_text = "Start Adventure!" in
  let btn_font_size = 20 in
  let btn_text_width = measure_text button_text btn_font_size in
  draw_text button_text
    (button_x + ((button_width - btn_text_width) / 2))
    (button_y + ((button_height - btn_font_size) / 2))
    btn_font_size Color.white;

  (* Draw Jim's character *)
  ignore
    (DefaultSkin.draw
       (float_of_int ((screen_width / 2) - 10))
       (float_of_int (button_y - 100)));

  (* Return if button was clicked *)
  if is_mouse_button_pressed MouseButton.Left then
    let mouse_pos = get_mouse_position () in
    let mouse_x = Vector2.x mouse_pos in
    let mouse_y = Vector2.y mouse_pos in
    mouse_x >= float_of_int button_x
    && mouse_x <= float_of_int (button_x + button_width)
    && mouse_y >= float_of_int button_y
    && mouse_y <= float_of_int (button_y + button_height)
  else false

let rec game_loop state player scroll_state =
  if window_should_close () then ()
  else begin
    begin_drawing ();

    let next_state =
      match state with
      | Onboarding ->
          if draw_onboarding () then (
            complete_onboarding player;
            LevelSelect)
          else Onboarding
      | LevelSelect ->
          clear_background Color.raywhite;
          Background.draw scroll_state.scroll_y screen_width screen_height;

          let points = get_path_points scroll_state.scroll_y in
          draw_path points;

          let clicked_level = ref None in
          for i = 0 to num_levels - 1 do
            let level_num = i + 1 in
            let x, y = points.(i) in
            draw_level_bubble player level_num (x, y);

            if is_mouse_button_pressed MouseButton.Left then begin
              let mouse_pos = get_mouse_position () in
              if
                check_level_click mouse_pos (x, y)
                && is_level_unlocked player level_num
              then clicked_level := Some level_num
            end
          done;

          update_scroll scroll_state;

          let shop_x, shop_y, shop_width, shop_height = draw_header () in
          let select_x, select_y, select_width, select_height =
            draw_skin_select_header ()
          in
          draw_title ();

          if is_mouse_button_pressed MouseButton.Left then begin
            let mouse_pos = get_mouse_position () in
            let mouse_x = Vector2.x mouse_pos in
            let mouse_y = Vector2.y mouse_pos in
            if
              mouse_x >= float_of_int shop_x
              && mouse_x <= float_of_int (shop_x + shop_width)
              && mouse_y >= float_of_int shop_y
              && mouse_y <= float_of_int (shop_y + shop_height)
            then Shop
            else if
              mouse_x >= float_of_int select_x
              && mouse_x <= float_of_int (select_x + select_width)
              && mouse_y >= float_of_int select_y
              && mouse_y <= float_of_int (select_y + select_height)
            then SkinSelect
            else
              match !clicked_level with
              | Some n -> Level n
              | None -> LevelSelect
          end
          else LevelSelect
      | Level n ->
          let obstacle_count = calculate_obstacle_count n in
          let starting_level = n in
          let speed_multiplier = int_of_float (2. +. (0.2 *. float_of_int n)) in
          let gravity = 1. +. (0.2 *. float_of_int n) in
          let jump_force = -18.0 -. (1. +. (1. *. float_of_int n)) in
          let biome = get_biome_for_level n in

          ignore
            (init_game starting_level obstacle_count speed_multiplier gravity
               jump_force biome player ());

          LevelSelect
      | Shop ->
          run_shop player;
          LevelSelect
      | SkinSelect ->
          run_skin_select player;
          LevelSelect
    in

    end_drawing ();
    game_loop next_state player scroll_state
  end

let create_scroll_state () =
  {
    scroll_y = 0.0;
    scroll_speed = 20.0;
    max_scroll = total_height -. view_height +. top_padding;
  }

let run_menu player =
  set_target_fps 60;

  let scroll_state = create_scroll_state () in
  let initial_state =
    if has_seen_onboarding player then LevelSelect else Onboarding
  in

  game_loop initial_state player scroll_state;
  close_window ()

let () =
  init_window screen_width screen_height "Jim's Adventure";
  let player = create_player () in
  run_menu player
