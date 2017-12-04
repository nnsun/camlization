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
  | City
  | Unit
  | Tech

type game_state = {
  player_turns: int;
  map: World.map ref;
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
    map = ref World.generate_map;
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

let game_map gst = !(gst.map)

let next_turn state =
  let player = state.players.(state.current_player) in
  let player = Player.set_gold player in
  let player = Player.set_science player in
  state.players.(state.current_player) <- player;
  {
    state with
    current_player = (state.current_player + 1) mod (Array.length state.players);
  }