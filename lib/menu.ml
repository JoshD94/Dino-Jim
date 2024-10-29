open Raylib
open Shop

let button_size = 100.0
let padding = 30.0
let grid_cols = 4
let num_levels = 12

(* Game state type *)
type game_state =
  | LevelSelect
  | Level of int
  | Shop

(* Run function to start the menu *)
let run_menu () =
  let screen_width = 1200 in
  let screen_height = 800 in
  init_window screen_width screen_height "Level Select";
  set_target_fps 60;

  let rec game_loop state =
    if window_should_close () then ()
    else begin
      begin_drawing ();

      (* Get next state based on current state and input *)
      let next_state =
        match state with
        | LevelSelect -> (
            clear_background Color.raywhite;

            (* Draw title *)
            let title = "Select Level" in
            let title_size = 40 in
            let title_width = measure_text title title_size in
            draw_text title
              ((screen_width - title_width) / 2)
              40 title_size Color.black;

            (* Draw shop button *)
            draw_rectangle 800 100 100 40 Color.gray;
            draw_text "Shop" 825 110 20 Color.white;

            (* Check for shop button click *)
            let shop_clicked = ref false in
            if is_mouse_button_pressed MouseButton.Left then begin
              let mouse_pos = get_mouse_position () in
              let mouse_x = Vector2.x mouse_pos in
              let mouse_y = Vector2.y mouse_pos in
              if
                mouse_x >= 800.0 && mouse_x <= 900.0 && mouse_y >= 100.0
                && mouse_y <= 140.0
              then shop_clicked := true
              else shop_clicked := false
            end;

            (* Draw level buttons *)
            let clicked_level = ref None in
            for i = 0 to num_levels - 1 do
              let row = float_of_int (i / grid_cols) in
              let col = float_of_int (i mod grid_cols) in
              let x =
                (800.0 -. (float_of_int grid_cols *. (button_size +. padding)))
                /. 2.0
                +. (col *. (button_size +. padding))
              in
              let y = 120.0 +. (row *. (button_size +. padding)) in

              (* Draw button *)
              let color = if i = 0 then Color.gray else Color.darkgray in
              draw_rectangle (int_of_float x) (int_of_float y)
                (int_of_float button_size) (int_of_float button_size) color;

              (* Draw number *)
              let number_str = string_of_int (i + 1) in
              let text_size = 30 in
              let text_width = measure_text number_str text_size in
              let text_x =
                x +. ((button_size -. float_of_int text_width) /. 2.0)
              in
              let text_y =
                y +. ((button_size -. float_of_int text_size) /. 2.0)
              in
              draw_text number_str (int_of_float text_x) (int_of_float text_y)
                text_size
                (if i = 0 then Color.white else Color.lightgray);

              (* Check for button click *)
              if is_mouse_button_pressed MouseButton.Left then begin
                let mouse_pos = get_mouse_position () in
                let mouse_x = Vector2.x mouse_pos in
                let mouse_y = Vector2.y mouse_pos in
                if
                  mouse_x >= x
                  && mouse_x <= x +. button_size
                  && mouse_y >= y
                  && mouse_y <= y +. button_size
                  && i = 0
                then clicked_level := Some (i + 1)
              end
            done;

            if !shop_clicked then Shop
            else
              match !clicked_level with
              | Some n -> Level n
              | None -> LevelSelect)
        | Level _ ->
            let starting_level = 1 in
            let starting_length = 1 in
            let starting_speed = 1 in
            let gravity = 1.0 in
            let jump_force = -18.0 in

            (* Run game *)
            ignore
              (Game.init_game starting_level starting_length starting_speed
                 gravity jump_force ());

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
