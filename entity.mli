open World

(* [entity] represents the various units/entities in the game *)
type entity

type city_entity

type unit_entity

type unit_class = Civilian | Military

type unit_type

(* [owner entity] is the player who owns [entity] *)
val owner : entity -> Player.player

(* [health entity] is the health of [entity] (>= 0) *)
val health : entity -> int

(* [tile entity] is the tile that [entity] is located on *)
val tile : entity -> int * int

(* [population city] is the population of [city] *)
val population : city_entity -> int

(* [is_capital city] is whether [city] is a capital *)
val is_capital : city_entity -> bool

(* [food_stock city] is the amount of food that [city] in storage *)
val food_stock : city_entity -> int

(* [unit_production city] is the unit that [city] is currently building *)
val unit_production : city_entity -> unit_type option

(* [production_stock city] is the amount of production that [city] has accumulated *)
val production_stock : city_entity -> int

(* [gold_per_turn city] is the amount of gold that [city] is generating per turn *)
val gold_per_turn : city_entity -> int

(* [science_per_turn city] is the amount of science that [city] is generating per turn *)
val science_per_turn : city_entity -> int

(* [production_per_turn city] is the amount of production that [city] is generating per turn *)
val production_per_turn : city_entity -> int

(* [food_per_turn city] is the amount of food that [city] is generating per turn *)
val food_per_turn : city_entity -> int

(* [movement_points unit] is the number of movement points that [unit] has per turn *)
val movement_points : unit_entity -> int

(* [strength unit] is the strength of [unit] *)
val strength : unit_entity -> int

(* [unit_cost unit] is the production cost of [unit] *)
val unit_cost : unit_entity -> int

(* [unit_class unit] is the unit class of [unit] (Civilian or Military) *)
val unit_class : unit_entity -> unit_class

(* [moves_left unit] is the remaining movement points of [unit] (>= 0) *)
val moves_left : unit_entity -> int
