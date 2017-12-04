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
val entities : player -> Entity.entity ref list

(* [set_gold player] calculates the new gold balance of [player] after a turn
   and returns the new state of player *)
val set_gold : player -> player

(* [set_science player] calculates the new science balance of [player] after a turn
 * and adds techs to [player]'s list of researched techs if they are finished being
 * researched *)
val set_science : player -> player

(* [set_production player] calculates production for each city, and creates new
 * if production has finished *)
val set_production : player -> player