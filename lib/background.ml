open Raylib

let grass_rgb = (206, 255, 194) (* Light green *)
let rock_rgb = (117, 117, 117) (* Light gray *)
let snow_rgb = (189, 251, 255) (* Light blue *)
let lava_rgb = (255, 158, 120) (* Light orange *)

(* Helper for linear interpolation of a single number *)
let lerp a b t =
  let t = max 0.0 (min 1.0 t) in
  (a *. (1.0 -. t)) +. (b *. t)

(* Get biome based on level number *)
let get_biome_for_level level =
  if level <= 3 then grass_rgb
  else if level <= 6 then rock_rgb
  else if level <= 9 then snow_rgb
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
    int_of_float (lerp (float_of_int r1) (float_of_int r2) transition_progress)
  in
  let g =
    int_of_float (lerp (float_of_int g1) (float_of_int g2) transition_progress)
  in
  let b =
    int_of_float (lerp (float_of_int b1) (float_of_int b2) transition_progress)
  in

  (* Draw the interpolated color *)
  draw_rectangle 0 0 screen_width screen_height (Color.create r g b 255)

let init () = ()
