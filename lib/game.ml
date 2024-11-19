open Raylib

type position = {
  x : float;
  y : float;
  velocity_y : float;
}

type game_state = {
  pos : position;
  obstacles : ObstacleType.obstacle list;
  speed : float;
  obstacles_passed : int;
  total_obstacles : int;
  completed : bool;
  died : bool;
  death_message : string;
}

(* Game constants *)
let level = 0
let initial_speed = 5.0
let jump_force = -20.0
let speed_up_multiplier = 1
let gravity = 0.8
let jump_height = 200.

(* Death messages *)
let get_death_message obstacle_name =
  match obstacle_name with
  | "cactus" -> "Jim got pricked by a cactus."
  | "rock" -> "Jim stumbled on a rock."
  | "tree" -> "Jim ran straight into a tree."
  | "bush" -> "Jim got tangled in a bush."
  | "snowman" -> "Jim doesn't like snowmen."
  | "ice" -> "Jim slipped on ice."
  | "evergreen" -> "Jim slammed into an evergreen."
  | "boulder" -> "Jim nearly died from a boulder."
  | "wall" -> "Jim hit a wall."
  | "lava" -> "Jim is not lava resistant."
  | "asteroid" -> "Jim stubbed his toe on an asteroid."
  | "fire" -> "Jim is no fireman."
  | "spike" -> "Jim got impaled on a spike."
  | _ -> "You Died!"

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

let update_position pos g jump_force =
  let is_on_ground = pos.y >= 350.0 in
  let pressed = is_key_pressed Key.Up || is_key_pressed Key.Space in

  let new_velocity =
    if pressed && is_on_ground then jump_force
    else if is_on_ground then 0.0
    else pos.velocity_y +. g
  in

  let new_y = pos.y +. new_velocity in

  if new_y > 350.0 then { x = pos.x; y = 350.0; velocity_y = 0.0 }
  else if new_y < 350. -. jump_height then
    { x = pos.x; y = 350. -. jump_height; velocity_y = 0.0 }
  else { x = pos.x; y = new_y; velocity_y = new_velocity }

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
  Obstacle.render state.obstacles;

  let pos = state.pos in
  (* current skin *)
  (Player.current_skin player) pos.x pos.y;

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
        let obstacle_rect =
          Rectangle.create obs.ObstacleType.x obs.ObstacleType.y
            obs.ObstacleType.width obs.ObstacleType.height
        in
        if check_collision_recs player_rect obstacle_rect then
          Some obs.ObstacleType.name
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

  (* initial game state *)
  let initial_state =
    {
      pos = { x = 100.0; y = 350.0; velocity_y = 0.0 };
      obstacles = Obstacle.init_obstacles biome obstacle_count ();
      speed = initial_speed;
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
    else if (state.completed || state.died) && check_retry_button () then ()
    else
      (* Update game state *)
      let new_pos = update_position state.pos gravity jump_force in
      let new_obstacles = Obstacle.update state.obstacles state.speed biome in

      (* get current passed count *)
      let obstacles_passed = Obstacle.get_passed_count () in

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
