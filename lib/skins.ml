open Raylib

module type Skin = sig
  val draw : float -> float -> unit
end

module DefaultSkin : Skin = struct
  let draw x y =
    let color = Color.gray in
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
end

module SantaJim : Skin = struct
  let draw x y =
    let color = Color.gray in
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
    draw_rectangle (int_of_float x + 18) (int_of_float y + 5) 3 3 Color.black;

    (* Santa hat *)
    let y = y +. 3. in
    draw_rectangle (int_of_float x - 2) (int_of_float y - 7) 10 10 Color.red;
    draw_rectangle (int_of_float x) (int_of_float y - 10) 30 10 Color.red;
    draw_rectangle (int_of_float x + 2) (int_of_float y - 15) 20 10 Color.red;
    draw_rectangle (int_of_float x + 3) (int_of_float y - 20) 10 10 Color.red;
    draw_circle (int_of_float x + 5) (int_of_float y - 23) 5. Color.black;
    draw_circle (int_of_float x + 5) (int_of_float y - 23) 2. Color.white
end

module AngryJim : Skin = struct
  let draw x y =
    let color = Color.gray in
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

    (* Eye *)
    draw_rectangle (int_of_float x + 18) (int_of_float y + 5) 3 3 Color.black
end
