type t = {
  mutable coins : int;
  mutable skins : string list;
  mutable jump_height : int;
  mutable speed : int;
  mutable coin_multiplier : int;
}

let create () =
  { coins = 0; skins = []; jump_height = 1; speed = 1; coin_multiplier = 1 }

let add_coins t coins = t.coins <- t.coins + coins
let add_skin t skin = t.skins <- skin :: t.skins

let add_powerup t power =
  if power = "Jump" then t.jump_height <- t.jump_height * 2
  else if power = "More coins" then t.coin_multiplier <- t.coin_multiplier * 2
  else if power = "Faster" then t.speed <- t.speed * 2

let skins t = t.skins
let coins t = t.coins
