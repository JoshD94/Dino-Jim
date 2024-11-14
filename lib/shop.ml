open Raylib
open Player

let button_height = 80.0
let button_width = 160.0
let padding = 120.0
let grid_cols = 4
let num_items = 7

let buyable_items =
  [|
    ("Jump", 0);
    ("Speed", 0);
    ("More coins", 0);
    ("Skin 1", 0);
    ("Skin 2", 2);
    ("Skin 3", 2);
    ("Chest", 10);
  |]

let buyable_skins =
  [| Skins.DefaultSkin.draw; Skins.DefaultSkin.draw; Skins.DefaultSkin.draw |]

let bought_items = [| false; false; false; false; false; false; false |]

type player_state = {
  mutable coins : int;
  mutable bought_items : string list;
}

(*let player = Player.create ()*)
let player = { coins = 10; bought_items = [] }

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
  let player = player in
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
        let row = float_of_int (i / grid_cols) in
        let col = float_of_int (i mod grid_cols) in
        let x =
          (1200.0 -. (float_of_int grid_cols *. (button_width +. padding)))
          /. 2.0
          +. (col *. (button_width +. padding))
        in
        let y = 160.0 +. (row *. (button_height +. padding)) in

        (* Draw button *)
        let color = if bought_items.(i) then Color.maroon else Color.red in
        draw_rectangle (int_of_float x) (int_of_float y)
          (int_of_float button_width)
          (int_of_float button_height)
          color;
        (* Draw Skins *)
        if i = 3 then Skins.DefaultSkin.draw (x +. 75.) (y -. 65.);
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
            bought_items.(i) <- true;
            if i < 3 then add_powerup state (fst buyable_items.(i));
            if i > 2 && i < 6 then add_skin state buyable_skins.(i - 3))
        end
      done;

      end_drawing ();
      game_loop player
    end
  in
  game_loop player
