open Primitives

(* [player] represents a user player in the game *)
type player

(* [new_player] is a newly created player *)
val new_player : player

(* [gold player] is the amount of gold player p has stored *)
val gold : player -> gold

(* [gold_rate player] is the amount of gold player is gaining per turn. Can be
 * negative *)
val gold_rate : player -> gold

(* [points player] is the amount of points player has accumulated thus far *)
val points : player -> int

(* [science player] is the amount of science player has accumulated thus far *)
val science : player -> science

(* [science_rate player] is the amount of science player accumulates per turn *)
val science_rate : player -> science

(* [techs player] is the set of technologies player has researched *)
val techs : player -> Tech.tech list
