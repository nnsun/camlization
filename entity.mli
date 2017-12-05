open World
open Tech

type city_entity

type unit_entity

(* [entity] represents the various units/entities in the game *)
type entity =
  | City of city_entity
  | Unit of unit_entity

type unit_class = Civilian | Military

type unit_type =
  | Worker
  | Scout
  | Warrior
  | WorkBoat
  | Archer
  | Trireme
  | Spearman
  | Chariot
  | Horseman
  | Swordsman
  | Catapult

(* [health entity] is the health of [entity] (>= 0) *)
val health : entity -> int

(* [tile entity] is the tile that [entity] is located on *)
val tile : entity -> tile

(* [is_city entity] is whether the entity represents a city *)
val is_city : entity -> bool

(* [is_unit entity] is whether the entity represents a unit *)
val is_unit : entity -> bool

(* [get_city_entity entity] returns the city_entity of [entity], if
 * [entity] represents a city, and generates an exception otherwise *)
val get_city_entity : entity -> city_entity

(* [get_unit_entity entity] returns the unit_entity of [entity], if
 * [entity] represents a unit, and generates an exception otherwise *)
val get_unit_entity : entity -> unit_entity

(* [growth_req pop] is how much food is required for a city's population to
 * increase at a given population *)
val growth_req : int -> int

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

(* [set_production city] calculates the new production stockpile value and builds a
 * new unit if production finishes
 * Returns: a tuple with a new city_entity with the updated hammer stock as the
 * first value and [Some unit_entity] if the unit_entity was built, or [None] if
 * production hasn't finished as it second value *)
val set_production : city_entity -> city_entity * unit_type option

(* [gold_per_turn city] is the amount of gold that [city] is generating per turn *)
val gold_per_turn : city_entity -> int

(* [science_per_turn city] is the amount of science that [city] is
 * generating per turn *)
val science_per_turn : city_entity -> int

(* [production_per_turn city] is the amount of production that [city] is
 * generating per turn *)
val production_per_turn : city_entity -> int

(* [food_per_turn city] is the amount of food that [city] is generating per turn *)
val food_per_turn : city_entity -> int

(* [movement_points unit] is the number of movement points that [unit] has per turn *)
val movement_points : unit_type -> int

(* [strength unit] is the strength of [unit] *)
val strength : unit_type -> int

(* [unit_cost unit] is the production cost of [unit] *)
val unit_cost : unit_type -> int

(* [unit_class unit] is the unit class of [unit] (Civilian or Military) *)
val unit_class : unit_type -> unit_class

(* [moves_left unit] is the remaining movement points of [unit] (>= 0) *)
val moves_left : unit_entity -> int

(* [new_unit unit_type tile] returns a new entity of type Unit with
 * unit type [unit_type] on tile [tile] *)
val new_unit : unit_type -> tile -> entity

(* [new_city tile] returns a new city of type City on tile [tile] *)
val new_city : tile -> entity

(* [set_growth city] calculates the new food stockpile and population
 * of [city] after a turn and returns the new city_entity *)
val set_growth : city_entity -> city_entity

(* [relative_str e1 e2] returns the relative strength of [e1] prior to
 * initiating combat with [e2] *)
val relative_str : entity -> entity -> float

(* [set_health entity i] returns [entity] with health set to [i] *)
val set_health : entity -> int -> entity

(* [get_unit_type u] is the unit type for [u] *)
val get_unit_type : unit_entity -> unit_type

(* [subtract_moves_left unit_entity cost] returns [entity] with cost fewer
 * moves left, or 0 moves left if the difference is less than 0 *)
val subtract_moves_left : unit_entity -> int -> unit_entity

(* [set_tile unit_entity tile] returns [unit_entity] with the tile location
 * set to [tile] *)
val set_tile : unit_entity -> tile -> unit_entity

(* [units_list] is the list of all [unit_type]s *)
val units_list : unit_type list

(* [tech_req utype] is [None] if [utype] doesn't have a tech requirement
 * and [Some t] if utype requires [t] to be researched to be built  *)
val tech_req : unit_type -> Tech.tech option

(* [set_food_per_turn n entity] returns [entity] with the food per turn set
 * to [n]
 * Requires: [entity] is a City *)
val set_food_per_turn : int -> entity -> entity

(* [set_food_per_turn n entity] returns [entity] with the production per turn set
 * to [n]
 * Requires: [entity] is a City *)
val set_production_per_turn : int -> entity -> entity

(* [set_food_per_turn n entity] returns [entity] with the gold per turn set
 * to [n]
 * Requires: [entity] is a City *)
val set_gold_per_turn : int -> entity -> entity

(* [reset_movement u] returns [u] with moves left reset to the full amount *)
val reset_movement : unit_entity -> unit_entity
