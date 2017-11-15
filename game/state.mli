(* [state] represents everything about the status of a game *)
type state

type time = int

(* [date state] is the amount of time in years surpassed so far in the game
 * represented by state *)
val date : state -> time

(* [map state] is the map of the game represented by state *)
val map : state -> Map.map
