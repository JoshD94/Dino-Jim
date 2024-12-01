open Raylib

let draw_default_with_color color x y =
  draw_rectangle (int_of_float x) (int_of_float y + 20) 20 30 color;

  (* Head *)
  draw_rectangle (int_of_float x) (int_of_float y + 5) 30 15 color;
  draw_rectangle (int_of_float x + 5) (int_of_float y) 25 20 color;

  (* Legs *)
  draw_rectangle (int_of_float x) (int_of_float y + 50) 5 10 color;
  draw_rectangle (int_of_float x + 15) (int_of_float y + 50) 5 10 color;

  (* Tail *)
  draw_rectangle (int_of_float x - 5) (int_of_float y + 35) 5 15 color;
  draw_rectangle (int_of_float x - 10) (int_of_float y + 40) 5 15 color;
  draw_rectangle (int_of_float x - 15) (int_of_float y + 45) 5 10 color;
  draw_rectangle (int_of_float x - 20) (int_of_float y + 50) 5 5 color;

  (* Eye *)
  draw_rectangle (int_of_float x + 18) (int_of_float y + 5) 3 3 Color.black

module type Skin = sig
  val draw : float -> float -> int
end

module DefaultSkin : Skin = struct
  let draw x y =
    draw_default_with_color Color.gray x y;
    (* Unique Skin Identifier *)
    0
end

module SantaJim : Skin = struct
  let draw x y =
    draw_default_with_color Color.gray x y;

    (* Santa hat *)
    let y = y +. 3. in
    draw_rectangle (int_of_float x - 2) (int_of_float y - 7) 10 10 Color.red;
    draw_rectangle (int_of_float x) (int_of_float y - 10) 30 10 Color.red;
    draw_rectangle (int_of_float x + 2) (int_of_float y - 15) 20 10 Color.red;
    draw_rectangle (int_of_float x + 3) (int_of_float y - 20) 10 10 Color.red;
    draw_circle (int_of_float x + 5) (int_of_float y - 23) 5. Color.black;
    draw_circle (int_of_float x + 5) (int_of_float y - 23) 2. Color.white;
    (* Unique Skin Identifier *)
    1
end

module AngryJim : Skin = struct
  let draw x y =
    draw_default_with_color Color.gray x y;

    (* Eyebrows *)
    draw_rectangle (int_of_float x + 15) (int_of_float y + -2) 3 3 Color.black;
    draw_rectangle
      (int_of_float (x +. 16.5))
      (int_of_float y + -1)
      3 3 Color.black;

    draw_rectangle (int_of_float x + 18) (int_of_float y + 0) 3 3 Color.black;
    draw_rectangle
      (int_of_float (x +. 19.5))
      (int_of_float y + 1)
      3 3 Color.black;

    draw_rectangle (int_of_float x + 21) (int_of_float y + 2) 3 3 Color.black;
    (* Unique Skin Identifier *)
    2
end

module GreenJim : Skin = struct
  let draw x y =
    draw_default_with_color Color.green x y;
    (* Unique Skin Identifier *)
    3
end

module RedJim : Skin = struct
  let draw x y =
    draw_default_with_color Color.red x y;
    (* Unique Skin Identifier *)
    4
end

module BlueJim : Skin = struct
  let draw x y =
    draw_default_with_color Color.blue x y;
    (* Unique Skin Identifier *)
    5
end

module InvisibleJim : Skin = struct
  let draw x y =
    draw_default_with_color Color.white x y;
    (* Unique Skin Identifier *)
    6
end

module OrangeJim : Skin = struct
  let draw x y =
    draw_default_with_color Color.orange x y;
    (* Unique Skin Identifier *)
    7
end

module DarthJim : Skin = struct
  let draw x y =
    draw_default_with_color Color.black x y;
    (* Eye *)
    draw_rectangle (int_of_float x + 18) (int_of_float y + 5) 3 3 Color.red;
    (* Unique Skin Identifier *)
    8
end
