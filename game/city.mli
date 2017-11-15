(* [city] represents a city in the game *)
type city

(* [owner city] is the player who owns [city] *)
val owner : city -> Player.player

(* [capital city] is whether [city] is the capital or not *)
val capital : city -> bool

(* [health city] is the health of [city] (>= 0) *)
val health : city -> int
