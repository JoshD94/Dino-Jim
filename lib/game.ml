open Raylib

(* Type definitions *)
type position = {
  x : float;
  y : float;
  velocity_y : float;
}

(* Game constants *)
let level = 0
let length = 0
let speed_up_multiplier = 0
let color = Color.gray
let jump_force = -20.0
let gravity = 0.8
let jump_height = 200.

(* Helper functions *)
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

let render pos =
  begin_drawing ();
  clear_background Color.raywhite;

  (* Ground line *)
  draw_line 0 410 1200 410 Color.black;

  (* Main body *)
  draw_rectangle (int_of_float pos.x) (int_of_float pos.y + 20) 20 30 color;

  (* Head *)
  draw_rectangle (int_of_float pos.x) (int_of_float pos.y + 5) 30 15 color;
  draw_rectangle (int_of_float pos.x + 5) (int_of_float pos.y) 25 20 color;

  (* Legs *)
  draw_rectangle (int_of_float pos.x) (int_of_float pos.y + 50) 5 10 color;
  draw_rectangle (int_of_float pos.x + 15) (int_of_float pos.y + 50) 5 10 color;

  (* Tail *)
  draw_rectangle (int_of_float pos.x - 5) (int_of_float pos.y + 35) 5 15 color;
  draw_rectangle (int_of_float pos.x - 10) (int_of_float pos.y + 40) 5 15 color;
  draw_rectangle (int_of_float pos.x - 15) (int_of_float pos.y + 45) 5 10 color;
  draw_rectangle (int_of_float pos.x - 20) (int_of_float pos.y + 50) 5 5 color;

  (* Eye *)
  draw_rectangle
    (int_of_float pos.x + 18)
    (int_of_float pos.y + 5)
    3 3 Color.black;

  end_drawing ()

(* Main game initialization and loop *)
let init_game lvl len speed_mul gravity jump_force () =
  (* Initialize window *)
  init_window 1200 800
    (Printf.sprintf "Level %d - Length %d - Speed %d" lvl len speed_mul);
  set_target_fps 60;

  (* Initial game state *)
  let initial_pos = { x = 100.0; y = 350.0; velocity_y = 0.0 } in
  let jump_force = jump_force in

  (* Main game loop *)
  let rec game_loop pos =
    if window_should_close () then close_window ()
    else begin
      (* Update game state *)
      let new_pos = update_position pos gravity jump_force in

      (* Render *)
      render new_pos;

      (* Continue loop *)
      game_loop new_pos
    end
  in

  (* Start the game loop *)
  game_loop initial_pos
