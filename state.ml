open Primitives

type multiplayer_options = {
  player_count_menu_open: bool;
  player_count: int
}

type menu_state =
  | Loading
  | Copyright
  | Main
  | Multiplayer of multiplayer_options
  | Options
  | About

type game_state = {
  date: time;
  map: World.map ref;
  map_display: int * int;
}

type state =
  | Menu of menu_state
  | Game of game_state
  | Quit

let start_state = Menu (Loading)

let date gst =
  let date = gst.date in
  let suffix = if date < 0 then " BCE" else " CE" in
  string_of_int date ^ suffix

let game_map gst = !(gst.map)