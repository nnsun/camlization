(* [city] represents a city in the game *)
type city

(* [owner city] is the player who owns [city] *)
val owner : city -> Player.player

(* [capital city] is whether [city] is the capital or not *)
val capital : city -> bool

(* [health city] is the health of [city] (>= 0) *)
val health : city -> int

(* [gold_per_turn city] is the gold produced per turn by [city] *)
val gold_per_turn : city -> int

(* [science_per_turn city] is the science produced per turn by [city] *)
val science_per_turn : city -> int
