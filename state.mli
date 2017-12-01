(* [state] represents everything about the status of the game *)
type state

(* [menu_state] represents the state of the main menu *)
type menu_state

(* [game_state] represents the state of a running game *)
type game_state

(* [gold] is an amount of gold coins *)
type gold = int

(* [date state] is the human-readable year (CE or BCE) of state [state] *)
val date : game_state -> string

(* [game_map state] is Some of the map of the game represented by state if the state
 * is in a game. None otherwise. *)
val game_map : game_state -> World.map
