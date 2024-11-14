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
