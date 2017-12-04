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

(* [maintenance player] is the amount of maintenance per turn needed to support
 * [player]'s units and cities *)
val maintenance : player -> gold

(* [points player] is the amount of points player has accumulated thus far *)
val points : player -> int

(* [science player] is the amount of science player has accumulated thus far *)
val science : player -> science

(* [science_rate player] is the amount of science player accumulates per turn *)
val science_rate : player -> science

(* [techs player] is the list of technologies that [player] has researched *)
val techs : player -> Tech.tech list

(* [entities player] is the list of entities that [player] owns *)
val entities : player -> Entity.entity list

(* [set_gold player] calculates the new gold balance of [player] after a turn *)
val set_gold : player -> player
