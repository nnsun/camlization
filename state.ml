open Primitives

type multiplayer_options = {
  player_count: int
}

type menu_state =
  | Loading
  | Copyright
  | Main of int
  | Multiplayer of int * multiplayer_options
  | Options
  | About

type pane_state =
  | Tile
  | City of int
  | Unit of int
  | Tech of int

type game_state = {
  player_turns: int;
  map: World.map;
  map_display: int * int;
  selected_tile: int * int;
  pane_state: pane_state;
  current_player: int;
  players: Player.player array;
}

type state =
  | Menu of menu_state
  | Game of game_state
  | Quit

let initial_year = -3000
let years_per_turn = 100

let start_state = Menu (Loading)

let initial_game_state options =
  {
    player_turns = 0;
    map = World.generate_map;
    map_display = (24, 24);
    selected_tile = (29, 25);
    pane_state = Tile;
    current_player = 0;
    players = Array.make options.player_count Player.new_player
  }

let turns gst = gst.player_turns mod Array.length gst.players

let date gst =
  let date = initial_year + (turns gst * years_per_turn) in
  let suffix = if date < 0 then " BCE" else " CE" in
  string_of_int (abs date) ^ suffix

let game_map gst = gst.map

let next_turn state =
  let player = state.players.(state.current_player) in
  let player = Player.set_gold player in
  let player = Player.set_science player in
  let player = Player.set_production player in
  let player = Player.set_growth player in
  state.players.(state.current_player) <- player;
  {
    state with
    current_player = (state.current_player + 1) mod (Array.length state.players);
  }

let entities coordinates gst =
  let valid_entity e =
    let entity = !e in
    Entity.health entity != 0
    && (World.coordinates (Entity.tile entity) = coordinates)
  in
  let entities_of_player p = List.filter valid_entity (Player.entities p) in
  let players = Array.map entities_of_player gst.players in
  players |> Array.to_list |> List.flatten |> List.map (!)

let units coordinates gst =
  let l = entities coordinates gst in
  let unit_entity e =
    Entity.(
      match e with
      | Unit u -> [u]
      | City c -> []
    )
  in
  List.map unit_entity l |> List.flatten

let city coordinates gst =
  let l = entities coordinates gst in
  try
    let entity = List.hd l in
    Entity.(
      match entity with
      | City c -> Some c
      | Unit u -> None
    )
  with _ -> None

let rec satisfies_pred pred lst =
  match lst with
  | [] -> None
  | a::b ->
    if pred a then Some a else satisfies_pred pred b

let tile_contains_enemy state tile =
  let player = state.players.(state.current_player) in
  let rec cycle_players players =
    match players with
    | [] -> None
    | a::b ->
      if a = player then cycle_players b
      else
        let entities_ref_list = Player.entities a in
        let is_on_tile t unit_ref =
          World.coordinates (Entity.tile !unit_ref) = World.coordinates t in
        let sat_pred_result = satisfies_pred (is_on_tile tile) entities_ref_list in
        if sat_pred_result <> None then sat_pred_result
        else cycle_players b in
  cycle_players (Array.to_list state.players)

let combat e1 e2 = failwith "Unimplemented"


let make_move state unit_entity_ref tile =
  let cost = World.movement_cost tile in
  let go_to_tile st e_ref t_ref = failwith "" in
    (* let opponent_opt = tile_contains_enemy st t_ref in
    match opponent_opt with
    | None ->
    | Some o -> *)

  if World.is_adjacent (Entity.tile (Entity.Unit !unit_entity_ref)) tile then
    if World.elevation tile = World.Peak then state
    else if World.terrain tile = World.Ice then state
    else if World.terrain tile = World.Ocean &&
            World.terrain tile = World.Coast then
      let player = state.players.(state.current_player) in
      let techs = Player.techs player in
      let is_optics tech = if tech = Tech.Optics then true else false in
      if not (List.exists is_optics techs) then state
      else go_to_tile state unit_entity_ref tile
    else
      go_to_tile state unit_entity_ref tile
  else state