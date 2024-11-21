type obstacle = {
  x : float;
  y : float;
  width : float;
  height : float;
  name : string;
}

let string_of_obstacle { x; y; width; height; name } =
  Printf.sprintf "{ x = %f; y = %f; width = %f; height = %f; name = %s }" x y
    width height name
