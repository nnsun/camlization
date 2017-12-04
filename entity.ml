open World
open Tech

(* [entity_info] represents information common to all entities:
* tile and health *)
type entity_info = {
  (* TODO: Check types *)
  health : int;
  tile : World.tile
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
  tech_req : Tech.tech option;
  res_req : World.resource option
}

let unit_attributes_map = [
  Worker, { movement = 2; strength = 0; cost = 100; uclass = Civilian;
              tech_req = None; res_req = None };
  Scout, { movement = 2; strength = 1; cost = 75; uclass = Military;
              tech_req = None; res_req = None };
  Warrior, { movement = 1; strength = 2; cost = 100; uclass = Military;
              tech_req = None; res_req = None };
  WorkBoat, { movement = 2; strength = 0; cost = 50; uclass = Civilian;
              tech_req = Some Fishing; res_req = None };
  Archer, { movement = 1; strength = 3; cost = 150; uclass = Military;
              tech_req = Some Archery; res_req = None };
  Trireme, { movement = 4; strength = 5; cost = 150; uclass = Military;
              tech_req = Some Sailing; res_req = None };
  Spearman, { movement = 1; strength = 4; cost = 175; uclass = Military;
              tech_req = Some BronzeWorking; res_req = None };
  Chariot, { movement = 2; strength = 4; cost = 175; uclass = Military;
              tech_req = Some TheWheel; res_req = Some Horses };
  Horseman, { movement = 2; strength = 7; cost = 300; uclass = Military;
              tech_req = Some HorsebackRiding; res_req = Some Horses };
  Swordsman, { movement = 1; strength = 6; cost = 250; uclass = Military;
              tech_req = Some IronWorking; res_req = Some Iron };
  Catapult, { movement = 1; strength = 5; cost = 175; uclass = Military;
              tech_req = Some Mathematics; res_req = None };
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
}

type city_entity = entity_info * city_info

type unit_entity = entity_info * unit_info

type entity =
  | City of city_entity
  | Unit of unit_entity

let shared_info e =
  match e with
  | City c -> fst c
  | Unit u -> fst u

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

let gold_per_turn city = failwith "Unimplemented"

let science_per_turn city = (snd city).population * 2

let production_per_turn city = failwith "Unimplemented"

let food_per_turn city = failwith "Unimplemented"

let movement_points utype = (List.assoc utype unit_attributes_map).movement

let strength utype = (List.assoc utype unit_attributes_map).strength

let unit_cost utype = (List.assoc utype unit_attributes_map).cost

let unit_class utype = (List.assoc utype unit_attributes_map).uclass

let moves_left unit = (snd unit).moves_left

let set_production city =
  let stock = (snd city).production_stock + production_per_turn city in
  let unit_production = (snd city).unit_production in
  let finished_flag = ref false in
  let new_city_info =
    match unit_production with
    | None -> { (snd city) with production_stock = stock }
    | Some u ->
      let cost = unit_cost u in
      if stock >= cost then
        let _ = finished_flag := true in
        { (snd city) with
          production_stock = 0;
          unit_production = None
        }
      else { (snd city) with production_stock = stock } in
  let output =
    if !finished_flag then unit_production else None in
  ((fst city, new_city_info), output)

let new_unit utype tile =
  Unit (
    { health = 100; tile = tile },
    {
      name = utype;
      moves_left = movement_points utype
    }
  )

let set_growth city =
  let stock = (snd city).food_stock + food_per_turn city in
  let pop = (snd city).population in
  let growth_req = 15 + 5 * pop in
  if stock >= growth_req then
    (fst city,
      { (snd city) with food_stock = stock - growth_req; population = pop })
  else
    (fst city, { (snd city) with food_stock = stock })

let relative_str e1 e2 =
  (* TODO: Add unit bonuses *)
  let (health, str) =
    match e1 with
    | City c ->
      ((fst c).health, (snd c).population)
    | Unit u ->
      ((fst u).health, (List.assoc  ((snd u).name) unit_attributes_map).strength) in
  (float_of_int health) /. 100. *. (float_of_int str)

let set_health entity new_health =
  let entity_info = { (shared_info entity) with health = new_health } in
  match entity with
  | City c ->
    City (entity_info, snd c )
  | Unit u ->
    Unit (entity_info, snd u)