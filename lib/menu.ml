open Raylib
open Shop

type game_state =
  | LevelSelect
  | Level of int
  | Shop

type scroll_state = {
  mutable scroll_y : float;
  scroll_speed : float;
  max_scroll : float;
  view_height : float;
  total_height : float;
}

let button_size = 100.0
let num_levels = 12
let view_height = 800.0
let level_spacing = 266.7
let total_height = float_of_int num_levels *. level_spacing
let screen_width = 1200
let screen_height = 800
let top_padding = 90.0

let get_path_points scroll_y =
  Array.init num_levels (fun i ->
      let base_y = float_of_int i *. level_spacing in
      let adjusted_y = base_y -. scroll_y +. top_padding in
      (* Added top padding *)
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

let draw_title () =
  let title = "Level Select" in
  let title_size = 40 in
  let title_width = measure_text title title_size in

  (* Draw a white rectangle behind the title *)
  let padding = 20 in
  let bg_rect_width = title_width + (padding * 2) in
  let bg_rect_height = title_size + padding in
  let bg_rect_x = (screen_width - bg_rect_width) / 2 in
  let bg_rect_y = 40 - (padding / 2) in

  draw_rectangle bg_rect_x bg_rect_y bg_rect_width bg_rect_height Color.raywhite;
  draw_text title ((screen_width - title_width) / 2) 40 title_size Color.black

let draw_level_bubble level_num (x, y) unlocked =
  let radius = button_size /. 2.0 in

  (* Draw shadow *)
  draw_circle
    (int_of_float (x +. 2.0))
    (int_of_float (y +. 2.0))
    radius (Color.create 0 0 0 40);

  (* Draw main circle with biome-specific colors *)
  let color =
    if not unlocked then Color.lightgray
    else
      match (level_num - 1) / 3 with
      | 0 -> Color.create 34 139 34 255 (* Forest Green *)
      | 1 -> Color.create 218 165 32 255 (* Desert Gold *)
      | 2 -> Color.create 135 206 235 255 (* Ice Blue *)
      | _ -> Color.create 178 34 34 255 (* Volcano Red *)
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
    text_color

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

(* menu.ml *)
let run_menu () =
  init_window screen_width screen_height "Level Select";
  set_target_fps 60;

  let scroll_state =
    {
      scroll_y = 0.0;
      scroll_speed = 20.0;
      max_scroll = total_height -. view_height +. top_padding;
      view_height;
      total_height;
    }
  in

  let rec game_loop state =
    if window_should_close () then ()
    else begin
      begin_drawing ();

      let next_state =
        match state with
        | LevelSelect ->
            clear_background Color.raywhite;

            (* Draw the pixel art background *)
            Background.draw scroll_state.scroll_y screen_width screen_height;

            let points = get_path_points scroll_state.scroll_y in
            draw_path points;

            let clicked_level = ref None in
            for i = 0 to num_levels - 1 do
              let level_num = i + 1 in
              let unlocked = true in
              let x, y = points.(i) in
              draw_level_bubble level_num (x, y) unlocked;

              if is_mouse_button_pressed MouseButton.Left then begin
                let mouse_pos = get_mouse_position () in
                if check_level_click mouse_pos (x, y) && unlocked then
                  clicked_level := Some level_num
              end
            done;

            update_scroll scroll_state;

            let shop_x, shop_y, shop_width, shop_height = draw_header () in
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
              else
                match !clicked_level with
                | Some n -> Level n
                | None -> LevelSelect
            end
            else LevelSelect
        | Level _ ->
            let starting_level = 1 in
            let starting_length = 1 in
            let starting_speed = 1 in
            let gravity = 1.0 in
            let jump_force = -18.0 in

            ignore
              (Game.init_game starting_level starting_length starting_speed
                 gravity jump_force "lava" ());

            LevelSelect
        | Shop ->
            run_shop ();
            LevelSelect
      in

      end_drawing ();
      game_loop next_state
    end
  in
  game_loop LevelSelect
