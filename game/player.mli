(* [player] represents a player in the game *)
type player

(* Amount of gold a player has stored *)
val gold : player -> int

(* Amount of gold a player is gaining per turn. Can be negative *)
val gold_rate : player -> int

(* Amount of points a player has accumulated thus far *)
val points : player -> int

(* Science the player has accumulated thus far *)
val science : player -> int

(* Science the player accumulates per turn *)
val science_rate : player -> int

(* Technologies the player has researched *)
val techs : player -> Tech.tech list
