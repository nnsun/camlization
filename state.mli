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
  | Acknowledgements

(* [pane_state] represents the state of the left pane,
 * including the current selection indices in each pane *)
type pane_state =
  | City of int
  | Unit of int * int
  | Tech of int

(* [game_state] represents the state of a running game *)
type game_state = {
  player_turns : int;
  map : World.map;
  map_display : int * int;
  selected_tile : int * int;
  pane_state : pane_state;
  current_player : int;
  players : Player.player array;
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

(* [game_map gst] is the map of the game represented by [gst] *)
val game_map : game_state -> World.map

(* [entities_refs coord gst] is the assoc list of player to entity refs at the
 * coordinates [coord] for the given game state [gst] *)
val entities_refs : int * int -> game_state -> (Player.player * Entity.entity ref) list

(* [entities coord gst] is the assoc list of player to entities at the coordinates
 * [coord] for the given game state [gst] *)
val entities : int * int -> game_state -> (Player.player * Entity.entity) list

(* [units coord gst] is the list of player * units at the coordinates [coord]
 * *)
val units : int * int -> game_state -> (Player.player * Entity.unit_entity) list

 (* [unit_refs coord gst] is the assoc list of player to unit refs at the
  * coordinates [coord] *)
val unit_refs : int * int -> game_state -> (Player.player * Entity.entity ref) list

(* [city coord gst] is the player * city at the coordinates [coord] if such
 * exists, or None otherwise *)
val city : int * int -> game_state -> (Player.player * Entity.city_entity) option

(* [city_ref coord gst] is the player * entity ref at the coordinates [coord]
 * if such exists, or None otherwise *)
val city_ref : int * int -> game_state -> (Player.player * Entity.entity ref) option

(* [make_move game_state unit tile] returns the resultant game state from attempting to
 * move [unit] to [tile].
 * Returns  [game_state, false] if move cannot be made, and [game_state, true] if
 * the move is legal, with [game_state] being the new state *)
val make_move : game_state -> Entity.entity ref -> World.tile -> game_state * bool

(* [available_techs state] returns a list of techs that the current player
 * is able to research *)
val available_techs : game_state -> Tech.tech list

(* [available_techs state] returns a list of [unit_type]s that the current player
 * is able to build *)
val available_units : game_state -> Entity.unit_type list

(* [next_turn gst] is the new state of the game after the current player
 * ends their turn *)
val next_turn : game_state -> game_state

(* [try_founding_city state tile entity] founds a city at [tile] if there are no cities
   within three tiles of [tile] *)
val try_founding_city : game_state -> World.tile -> Entity.entity ref -> game_state

(* [player_number gst p] is the player number of the player [p] in game_state
 * [gst] *)
val player_number : game_state -> Player.player -> int