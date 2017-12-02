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

type game_state = {
  date: time;
  map: World.map ref;
  map_display: int * int;
  selected_tile: int * int;
  current_player: int;
  players: Player.player array;
}

type state =
  | Menu of menu_state
  | Game of game_state
  | Quit

let start_state = Menu (Loading)

let initial_game_state options =
  {
    date = -3000;
    map = ref World.generate_map;
    map_display = (24, 24);
    selected_tile = (29, 25);
    current_player = 0;
    players = Array.make options.player_count Player.new_player
  }

let date gst =
  let date = gst.date in
  let suffix = if date < 0 then " BCE" else " CE" in
  string_of_int date ^ suffix

let game_map gst = !(gst.map)