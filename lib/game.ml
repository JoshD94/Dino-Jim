open Raylib

type position = {
  x : float;
  y : float;
  velocity_y : float;
}

type obstacle = Obstacle.t

type game_state = {
  pos : position;
  obstacles : obstacle list;
  speed : float;
  obstacles_passed : int;
  total_obstacles : int;
  completed : bool;
  died : bool;
  death_message : string;
}

let jump_height = 200.

(* Death messages *)
let get_death_message obstacle_name =
  match obstacle_name with
  | "cactus" -> "Jim got pricked by a cactus."
  | "rock" -> "Jim stumbled on a rock."
  | "tree" -> "Jim ran straight into a tree."
  | "bush" -> "Jim got tangled in a bush."
  | "snowman" -> "Jim doesn't like snowmen."
  | "ice" -> "Jim slipped on ice."
  | "evergreen" -> "Jim slammed into an evergreen."
  | "boulder" -> "Jim nearly died from a boulder."
  | "wall" -> "Jim hit a wall."
  | "lava" -> "Jim is not lava resistant."
  | "asteroid" -> "Jim stubbed his toe on an asteroid."
  | "fire" -> "Jim is no fireman."
  | "spike" -> "Jim got impaled on a spike."
  | _ -> "You Died!"

let update_position (pos : position) g jump_force =
  let is_on_ground = pos.y >= 350.0 in
  let pressed = is_key_pressed Key.Up || is_key_pressed Key.Space in

  let new_velocity =
    if pressed && is_on_ground then jump_force
    else if is_on_ground then 0.0
    else pos.velocity_y +. g
  in

  let new_y = pos.y +. new_velocity in

  if new_y > 350.0 then { x = pos.x; y = 350.0; velocity_y = 0.0 }
  else if new_y < 350. -. jump_height then
    { x = pos.x; y = 350. -. jump_height; velocity_y = 0.0 }
  else { x = pos.x; y = new_y; velocity_y = new_velocity }

let get_level_reward level =
  if level <= 3 then 5 (* Forest levels: +5 coins *)
  else if level <= 6 then 10 (* Snow levels: +10 coins *)
  else if level <= 9 then 15 (* Rock levels: +15 coins *)
  else 20 (* Volcano levels: +20 coins *)
