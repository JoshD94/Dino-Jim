open Raylib

(* Type definitions *)
type position = {
  x : float;
  y : float;
  velocity_y : float;
}

(* Game state type - add obstacles *)
type game_state = {
  pos : position;
  obstacles : Obstacle.t list;
  speed : float;
}

(* Game constants *)
let level = 0
let length = 0
let initial_speed = 5.0
let jump_force = -20.0
let speed_up_multiplier = 1
let gravity = 0.8
let jump_height = 200.

(* Helper functions *)
let check_back_button () =
  if is_mouse_button_pressed MouseButton.Left then
    let mouse_pos = get_mouse_position () in
    let mouse_x = Vector2.x mouse_pos in
    let mouse_y = Vector2.y mouse_pos in
    mouse_x >= 20.0 && mouse_x <= 120.0 && mouse_y >= 20.0 && mouse_y <= 60.0
  else false

let update_position pos g jump_force =
  let is_on_ground = pos.y >= 350.0 in
  let pressed = is_key_pressed Key.Up || is_key_pressed Key.Space in

  (* Calculate new velocity *)
  let new_velocity =
    if pressed && is_on_ground then jump_force (* Initial jump velocity *)
    else if is_on_ground then 0.0 (* Reset velocity when on ground *)
    else pos.velocity_y +. g (* Apply gravity to velocity *)
  in

  (* Calculate new position based on velocity *)
  let new_y = pos.y +. new_velocity in

  (* Apply constraints *)
  if new_y > 350.0 then { x = pos.x; y = 350.0; velocity_y = 0.0 }
    (* Hit ground *)
  else if new_y < 350. -. jump_height then
    { x = pos.x; y = 350. -. jump_height; velocity_y = 0.0 } (* Hit ceiling *)
  else { x = pos.x; y = new_y; velocity_y = new_velocity }

let render state player =
  begin_drawing ();
  clear_background Color.raywhite;

  (* Ground line *)
  draw_line 0 410 1200 410 Color.black;

  (* Render obstacles *)
  Obstacle.render state.obstacles;

  let pos = state.pos in

  (* Draw current skin *)
  (Player.current_skin player) pos.x pos.y;

  (* Draw back button last so it's always on top *)
  draw_rectangle 20 20 100 40 Color.gray;
  draw_text "Back" 45 30 20 Color.white;

  end_drawing ()

(* Main game initialization and loop *)
let init_game lvl len speed_mul gravity jump_force biome player () =
  print_endline
    (string_of_int lvl ^ " " ^ string_of_int len ^ " " ^ string_of_int speed_mul
   ^ " " ^ string_of_float gravity ^ " " ^ string_of_float jump_force);
  (* Initial game state *)
  let initial_state =
    {
      pos = { x = 100.0; y = 350.0; velocity_y = 0.0 };
      obstacles = Obstacle.init_obstacles biome ();
      speed = initial_speed;
    }
  in

  (* Main game loop *)
  let rec game_loop state player =
    if window_should_close () then ()
    else if check_back_button () then ()
    else
      (* Update game state *)
      let new_pos = update_position state.pos gravity jump_force in
      let new_obstacles = Obstacle.update state.obstacles state.speed biome in

      (* Check for collision *)
      let collided =
        Obstacle.check_collision new_pos.x new_pos.y 30.0 60.0 new_obstacles
      in

      if collided then ()
      else begin
        (* Update state and continue *)
        let new_state =
          { pos = new_pos; obstacles = new_obstacles; speed = state.speed }
        in

        (* Render *)
        render new_state player;

        (* Continue loop *)
        game_loop new_state player
      end
  in

  (* Start the game loop *)
  game_loop initial_state player
