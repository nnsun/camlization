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

type pane_state =
  | Tile
  | City of int
  | Unit of int
  | Tech of int

(* [game_state] represents the state of a running game *)
type game_state = {
  player_turns: int;
  map: World.map;
  map_display: int * int;
  selected_tile: int * int;
  pane_state: pane_state;
  current_player: int;
  players: Player.player array;
}

(* [state] represents everything about the status of the game *)
type state =
  | Menu of menu_state
  | Game of game_state
  | Quit

(* [start_state] is the state that the program begins in *)
val start_state : state

(* [initial_game_state options] is the state that a new multiplayer game starts
 * in with the given [options] *)
val initial_game_state : multiplayer_options -> game_state

(* [turns gst] is the number of times each player has taken a turn in the
 * game *)
val turns : game_state -> int

(* [date gst] is the human-readable year (CE or BCE) of state [state] *)
val date : game_state -> string

(* [game_map gst] is Some of the map of the game represented by state if the state
 * is in a game. None otherwise. *)
val game_map : game_state -> World.map

(* [next_turn gst] is the new state of the game after the current player
 * ends their turn *)
val next_turn : game_state -> game_state

(* [entities_refs coord gst] is the list of entity refs at the coordinates
 * [coord] for the given game state [gst] *)
val entities_refs : int * int -> game_state -> Entity.entity ref list

(* [entities coord gst] is the list of entities at the coordinates [coord] for
 * the given game state [gst] *)
val entities : int * int -> game_state -> Entity.entity list

(* [city coord gst] is the city at the coordinates [coord] if such exists, or
 * None otherwise *)
val city : int * int -> game_state -> Entity.city_entity option

(* [units coord gst] is the list of units at the coordinates [coord] if such
 * exists, or None otherwise *)
val units : int * int -> game_state -> Entity.unit_entity list

(* [unit_refs coord gst] is the list of unit refs at the coordinates [coord] if
 * such exists, or None otherwise *)
val unit_refs : int * int -> game_state -> Entity.entity ref list

(* [make_move game_state unit tile] returns the resultant game state from attempting to
 * move [unit] to [tile].
 * Returns  [game_state] if move cannot be made, and the resultant [game_state] if
 * the move is legal *)
val make_move : game_state -> Entity.entity ref -> World.tile -> game_state

(* [available_techs state] returns a list of techs that the current player
 * is able to research *)
val available_techs : game_state -> Tech.tech list

(* [available_techs state] returns a list of [unit_type]s that the current player
 * is able to build *)
val available_units : game_state -> Entity.unit_type list