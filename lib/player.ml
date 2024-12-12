open Skins

type t = {
  mutable has_seen_onboarding : bool;
  mutable current_skin : float -> float -> int;
  mutable coins : int;
  mutable skins : (float -> float -> int) list;
  mutable buyable_skins : (float -> float -> int) list;
  mutable completed_levels : int list;
}

let all_skins =
  [|
    DefaultSkin.draw;
    SantaJim.draw;
    AngryJim.draw;
    RedJim.draw;
    GreenJim.draw;
    BlueJim.draw;
    OrangeJim.draw;
    PurpleJim.draw;
    YellowJim.draw;
    InvisibleJim.draw;
    DarthJim.draw;
    MagentaJim.draw;
  |]

let save_filename = ref "save_game.csv"
let set_save_file filename = save_filename := filename

let save_state t =
  try
    let oc = open_out !save_filename in
    output_string oc "coins,completed_levels,has_seen_onboarding\n";
    Printf.fprintf oc "%d,%s,%b\n" t.coins
      (String.concat ";" (List.map string_of_int t.completed_levels))
      t.has_seen_onboarding;
    close_out oc
  with _ -> ()

let create_player () =
  let player =
    {
      has_seen_onboarding = false;
      current_skin = DefaultSkin.draw;
      coins = 10;
      skins = [ DefaultSkin.draw; SantaJim.draw; AngryJim.draw ];
      buyable_skins =
        [
          RedJim.draw;
          GreenJim.draw;
          BlueJim.draw;
          OrangeJim.draw;
          PurpleJim.draw;
          YellowJim.draw;
        ];
      completed_levels = [];
    }
  in
  (* Try to load saved progress *)
  (try
     let ic = open_in !save_filename in
     let _ = input_line ic in
     (* Skip header *)
     let saved_data = input_line ic in
     let fields = String.split_on_char ',' saved_data in
     (match fields with
     | coins :: completed_levels_str :: has_seen_onboarding_str :: _ ->
         player.coins <- int_of_string coins;
         player.completed_levels <-
           (if completed_levels_str = "" then []
            else
              List.map int_of_string
                (String.split_on_char ';' completed_levels_str));
         player.has_seen_onboarding <- bool_of_string has_seen_onboarding_str
     | _ -> ());
     close_in ic
   with _ -> ());
  player

let rec remove_from_buyable skin_list (skin : float -> float -> int) =
  match skin_list with
  | [] -> []
  | h :: t ->
      if h 10000. 10000. = skin 10000. 10000. then t
      else h :: remove_from_buyable t skin

let add_coins t amount =
  t.coins <- t.coins + amount;
  save_state t

let complete_onboarding t =
  t.has_seen_onboarding <- true;
  save_state t

let has_seen_onboarding t = t.has_seen_onboarding

let add_skin t skin =
  if List.filter (fun x -> x 10000. 10000. = skin 10000. 10000.) t.skins = []
  then (
    t.skins <- skin :: t.skins;
    t.buyable_skins <- remove_from_buyable t.buyable_skins skin)

let skins t = t.skins
let coins t = t.coins
let buyable_skin_list t = t.buyable_skins
let current_skin t = t.current_skin

let rec skin_in_list lst skin =
  match lst with
  | [] -> false
  | h :: t ->
      if h 10000. 10000. = skin 10000. 10000. then true else skin_in_list t skin

let has_skin t skin = skin_in_list t.skins skin

let select_skin t skin =
  if skin_in_list t.skins skin then t.current_skin <- skin

let is_level_unlocked t level =
  level = 1 || List.mem (level - 1) t.completed_levels

let complete_level t level =
  if not (List.mem level t.completed_levels) then begin
    t.completed_levels <- level :: t.completed_levels;
    save_state t
  end

let rec load_skins_help lst acc =
  match lst with
  | [] -> []
  | h :: t ->
      let temp = ref [] in
      for i = 0 to Array.length all_skins - 1 do
        if all_skins.(i) 10000. 10000. = h then temp := all_skins.(i) :: !temp
      done;
      load_skins_help t (!temp @ acc)

let load_skins lst = load_skins_help lst []
let get_completed_levels t = t.completed_levels
