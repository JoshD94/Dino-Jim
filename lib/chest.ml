open Skins

type t = {
  mutable skin_lst : (float -> float -> int) list;
  mutable skin : float -> float -> int;
}

let init_skins =
  [
    InvisibleJim.draw;
    DarthJim.draw;
    MagentaJim.draw;
    SantaJim.draw;
    AngryJim.draw;
  ]

let create_chest () =
  { skin_lst = List.tl init_skins; skin = List.hd init_skins }

let more_skins chest =
  chest.skin 10000. 10000. <> DefaultSkin.draw 10000. 10000.

let open_chest chest player =
  Player.add_skin player chest.skin;
  if
    chest.skin_lst <> []
    && chest.skin 10000. 10000. <> DefaultSkin.draw 10000. 10000.
  then (
    chest.skin <- List.hd chest.skin_lst;
    chest.skin_lst <- List.tl chest.skin_lst;
    if chest.skin_lst = [] then chest.skin_lst <- [ DefaultSkin.draw ])
