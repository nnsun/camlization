open World

(* [entity_info] represents information common to all entities,
* such as owner and health *)
type entity_info = {
  (* TODO: Check types *)
  health : int;
  tile : int * int
}

(* [unit_class] is the class of the unit: civilian or military unit *)
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

type unit_attributes = {
  movement : int;
  strength : int;
  cost : int;
  uclass : unit_class;
  res_req : World.resource option
}

let unit_attributes_map = [
  Worker, { movement = 2; strength = 0; cost = 100;
              uclass = Civilian; res_req = None };
  Scout, { movement = 2; strength = 1; cost = 75;
              uclass = Military; res_req = None };
  Warrior, { movement = 1; strength = 2; cost = 100;
              uclass = Military; res_req = None };
  WorkBoat, { movement = 2; strength = 0; cost = 50;
              uclass = Civilian; res_req = None };
  Archer, { movement = 1; strength = 3; cost = 150;
              uclass = Military; res_req = None };
  Trireme, { movement = 4; strength = 5; cost = 150;
              uclass = Military; res_req = None };
  Spearman, { movement = 1; strength = 4; cost = 175;
              uclass = Military; res_req = None };
  Chariot, { movement = 2; strength = 4; cost = 175;
              uclass = Military; res_req = Some Horses };
  Horseman, { movement = 2; strength = 7; cost = 300;
              uclass = Military; res_req = Some Horses };
  Swordsman, { movement = 1; strength = 6; cost = 250;
              uclass = Military; res_req = Some Iron };
  Catapult, { movement = 1; strength = 5; cost = 175;
              uclass = Military; res_req = None };
]


(* [unit_info] represents information specific to units, such as
* unit class and movement points *)
type unit_info = {
  name : unit_type;
  moves_left : int;
}

(* [city_info] represents information specific to cities,
* such as current build task, population, and gold output *)
type city_info = {
  population: int;
  is_capital : bool;
  food_stock: int;
  unit_production : unit_type option;
  production_stock : int;
  gold : int;
  science : int;
  production : int;
  food : int;
}

type city_entity = entity_info * city_info

type unit_entity = entity_info * unit_info

type entity =
  | City of city_entity
  | Unit of unit_entity

let health entity =
  match entity with
  | City e -> (fst e).health
  | Unit e -> (fst e).health

let tile entity =
  match entity with
  | City e -> (fst e).tile
  | Unit e -> (fst e).tile

let is_city entity =
  match entity with
  | City e -> true
  | Unit e -> false

let is_unit entity = not (is_city entity)

let get_city_entity entity =
  match entity with
  | City c -> c
  | _ -> failwith "Error: expected City but got Unit"

  let get_unit_entity entity =
    match entity with
    | Unit u -> u
    | _ -> failwith "Error: expected Unit but got City"

let population city = (snd city).population

let is_capital city = (snd city).is_capital

let food_stock city = (snd city).food_stock

let unit_production city = (snd city).unit_production

let production_stock city = (snd city).production_stock

let gold_per_turn city = (snd city).gold

let science_per_turn city = (snd city).science

let production_per_turn city = (snd city).production

let food_per_turn city = (snd city).food

let get_unit_attributes unit =
  let unit_name = (snd unit).name in
  List.assoc unit_name unit_attributes_map


let movement_points unit = (get_unit_attributes unit).movement

let strength unit = (get_unit_attributes unit).strength

let unit_cost unit = (get_unit_attributes unit).cost

let unit_class unit = (get_unit_attributes unit).uclass

let moves_left unit = (snd unit).moves_left