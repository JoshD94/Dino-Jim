open Raylib

(* Biome colors stored as (r,g,b) tuples *)
let forest_rgb = (34, 139, 34)
let desert_rgb = (218, 165, 32)
let ice_rgb = (135, 206, 235)
let volcano_rgb = (178, 34, 34)

(* Helper for linear interpolation of a single number *)
let lerp a b t =
  let t = max 0.0 (min 1.0 t) in
  (a *. (1.0 -. t)) +. (b *. t)

(* Draw the background with gradual color transitions *)
let draw scroll_y screen_width screen_height =
  let levels_per_biome = 3 in
  let level_height = float_of_int screen_height *. 0.333 in
  let biome_height = level_height *. float_of_int levels_per_biome in

  (* Calculate current biome and transition progress *)
  let biome_position = scroll_y /. biome_height in
  let current_biome = int_of_float biome_position in
  let transition_progress = biome_position -. float_of_int current_biome in

  (* Get current and next RGB values *)
  let current_rgb =
    match current_biome mod 4 with
    | 0 -> forest_rgb
    | 1 -> desert_rgb
    | 2 -> ice_rgb
    | _ -> volcano_rgb
  in

  let next_rgb =
    match (current_biome + 1) mod 4 with
    | 0 -> forest_rgb
    | 1 -> desert_rgb
    | 2 -> ice_rgb
    | _ -> volcano_rgb
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
