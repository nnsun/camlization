open Primitives

(* [player] represents a user player in the game *)
type player

(* [new_player] is a newly created player *)
val new_player : player

val new_player2 : player

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

(* [current_tech player] is [None] if [player] is not researching a tech and
 * [Some t] if [player] is researching t *)
val current_tech : player -> Tech.tech option

(* [entities player] is the list of refs of entities that [player] owns *)
val entities : player -> Entity.entity ref list

(* [player_owns_entity p e] returns whether player [p] has entity [e] *)
val player_owns_entity : player -> Entity.entity ref -> bool

(* [set_gold player] calculates the new gold balance of [player] after a turn
   and returns the new state of player *)
val set_gold : player -> player

(* [set_science player] calculates the new science balance of [player] after a turn
 * and adds techs to [player]'s list of researched techs if they are finished being
 * researched *)
val set_science : player -> player

(* [set_production player] calculates production for each city, and creates new
 * units if production has finished *)
val set_production : player -> player

(* [set_growth player] calculates growth for each city, and adjusts food
 * stockpile levels and population *)
 val set_growth : player -> player

(* [filter_city_refs player] is the list of refs of cities that [player] owns *)
val filter_city_refs : player -> Entity.entity ref list

(* [filter_unit_refs player] is the list of refs of units that [player] owns *)
val filter_unit_refs : player -> Entity.entity ref list

(* [research_tech player tech] updates the current research of [player] to [tech] *)
val research_tech : player -> Tech.tech -> player

(* [found_city player tile] creates a new city for [player] at [tile] *)
val found_city : player -> World.tile -> player