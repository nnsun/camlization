open Primitives

type multiplayer_options = {
  player_count: int
}

(* [menu_state] represents the state of the main menu *)
type menu_state =
  | Loading
  | Copyright
  | Main of int (* index *)
  | Multiplayer of int * multiplayer_options
  | Options
  | About

(* [game_state] represents the state of a running game *)
type game_state = {
  player_turns: int;
  map: World.map ref;
  map_display: int * int;
  selected_tile: int * int;
  current_player: int;
  players: Player.player array;
}

(* [state] represents everything about the status of the game *)
type state =
  | Menu of menu_state
  | Game of game_state
  | Quit

(* [state_state] is the state that the game begins in *)
val start_state : state

(* [initial_game_state options] is the state that a new multiplayer game starts
 * in with the given [options] *)
val initial_game_state : multiplayer_options -> game_state

(* [turns state] is the number of times each player has taken a turn in the
 * game *)
val turns : game_state -> int

(* [date state] is the human-readable year (CE or BCE) of state [state] *)
val date : game_state -> string

(* [game_map state] is Some of the map of the game represented by state if the state
 * is in a game. None otherwise. *)
val game_map : game_state -> World.map
