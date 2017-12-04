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
    && (World.coordinates !(Entity.tile entity) = coordinates)
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
