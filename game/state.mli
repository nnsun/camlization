type state

type time = int

(* The amount of time in years surpassed in the game so far *)
val date : state -> time

(* The map of the game *)
val map : state -> Map.map
