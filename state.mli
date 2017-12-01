open Primitives

type multiplayer_options = {
  player_count_menu_open: bool;
  player_count: int
}

(* [menu_state] represents the state of the main menu *)
type menu_state =
  | Loading
  | Copyright
  | Main of int (* index *)
  | Multiplayer of multiplayer_options
  | Options
  | About

(* [game_state] represents the state of a running game *)
type game_state = {
  date: time;
  map: World.map ref;
  map_display: int * int;
}

(* [state] represents everything about the status of the game *)
type state =
  | Menu of menu_state
  | Game of game_state
  | Quit

(* [state_state] is the state that the game begins in *)
val start_state : state

(* [date state] is the human-readable year (CE or BCE) of state [state] *)
val date : game_state -> string

(* [game_map state] is Some of the map of the game represented by state if the state
 * is in a game. None otherwise. *)
val game_map : game_state -> World.map
