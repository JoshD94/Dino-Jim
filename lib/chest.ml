type t = { skin : float -> float -> int }

let create player = { skin = List.hd (Player.buyable_skin_list player) }
let open_chest chest player = Player.add_skin player chest.skin
