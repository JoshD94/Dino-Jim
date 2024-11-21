type t = {
  mutable current_skin : float -> float -> unit;
  mutable coins : int;
  mutable skins : (float -> float -> unit) list;
  mutable jump_height : int;
  mutable speed : int;
  mutable coin_multiplier : int;
  mutable buyable_skins : (float -> float -> unit) list;
  mutable powerups : string list;
}

let create () =
  {
    current_skin = Skins.SantaJim.draw;
    coins = 10;
    skins = [ Skins.DefaultSkin.draw; Skins.SantaJim.draw; Skins.AngryJim.draw ];
    jump_height = 1;
    speed = 1;
    coin_multiplier = 1;
    buyable_skins = [];
    powerups = [];
  }

let rec remove_from_buyable skin_list skin =
  match skin_list with
  | [] -> []
  | h :: t -> if h = skin then t else h :: remove_from_buyable t skin

let add_coins t coins = t.coins <- t.coins + coins

let add_skin t skin =
  t.skins <- skin :: t.skins;
  t.buyable_skins <- remove_from_buyable t.buyable_skins skin

let add_powerup t power =
  if power = "Jump" then (
    t.jump_height <- t.jump_height * 2;
    t.powerups <- "Jump" :: t.powerups)
  else if power = "More coins" then (
    t.coin_multiplier <- t.coin_multiplier * 2;
    t.powerups <- "More coins" :: t.powerups)
  else if power = "Faster" then (
    t.speed <- t.speed * 2;
    t.powerups <- "Faster" :: t.powerups)

let rec in_list lst x =
  match lst with
  | [] -> false
  | h :: t -> if x = h then true else in_list t x

let has_powerup t power = in_list t.powerups power
let skins t = t.skins
let coins t = t.coins
let buyable_skin_list t = t.buyable_skins
let current_skin t = t.current_skin

let rec skin_in_list lst skin =
  match lst with
  | [] -> false
  | h :: t -> if h = skin then true else skin_in_list t skin

let select_skin t skin =
  if skin_in_list t.skins skin then t.current_skin <- skin
