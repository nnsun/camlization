(* [player] represents a user player in the game *)
type player

(* [gold player] is the amount of gold player p has stored *)
val gold : player -> int

(* [gold_rate player] is the amount of gold player is gaining per turn. Can be
 * negative *)
val gold_rate : player -> int

(* [points player] is the amount of points player has accumulated thus far *)
val points : player -> int

(* [science player] is the amount of science player has accumulated thus far *)
val science : player -> int

(* [science_rate player] is the amount of science player accumulates per turn *)
val science_rate : player -> int

(* [techs player] is the list of technologies player has researched *)
val techs : player -> Tech.tech list

