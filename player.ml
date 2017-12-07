open Primitives

type player = {
  gold: int;
  current_tech : Tech.tech option;
  science: science;
  techs: Tech.tech list;
  entities : Entity.entity ref list;
  num_cities : int;
  num_units : int
}

let new_player tile = {
  gold = 0;
  current_tech = None;
  science = 0;
  techs = [];
  entities = [ref (Entity.new_unit Entity.Worker tile)];
  num_cities = 0;
  num_units = 1;
}

let gold p = p.gold

let filter_city_refs p =
  let is_city_ref entity = Entity.is_city !entity in
  List.filter is_city_ref p.entities

let filter_unit_refs p =
  let is_unit_ref entity = Entity.is_unit !entity in
  List.filter is_unit_ref p.entities

let gold_rate p =
  let city_refs = filter_city_refs p in
  let add_gold acc city_ref =
    Entity.gold_per_turn (Entity.get_city_entity !city_ref) + acc in
  List.fold_left add_gold 0 city_refs

let maintenance p =
  let free_cities = 4 in
  let free_units = 4 in
  let city_maintenance =
    if p.num_cities > free_cities then
      10 * (p.num_cities - free_cities)
    else 0 in
  let unit_maintenance =
    if p.num_units > free_units then
      3 * (p.num_units - free_units)
    else 0 in
  city_maintenance + unit_maintenance

let points p = 100

let science p = p.science

let science_rate p =
  let city_refs = filter_city_refs p in
  let add_science acc city_ref =
    Entity.science_per_turn (Entity.get_city_entity !city_ref) + acc in
  List.fold_left add_science 0 city_refs

let techs p = p.techs

let current_tech p = p.current_tech

let entities p = p.entities

let player_owns_entity p e =
  List.exists (fun elt -> elt = e) p.entities

let set_gold p =
  { p with gold = p.gold + gold_rate p - maintenance p }

let set_science p =
  let new_science = p.science + science_rate p in
  match p.current_tech with
  | Some tech ->
    if new_science >= Tech.tech_cost tech then
      { p with science = 0; techs = tech::p.techs; current_tech = None }
    else { p with science = new_science }
  | _ -> p

let set_production p =
  let city_refs = filter_city_refs p in
  let rec cycle_cities refs player =
    match refs with
    | [] -> player
    | a::b ->
      let city_entity = (
        match !a with
        | Entity.City c -> c
        | _ -> failwith "Error: expected City but got Unit"
      ) in
      let (city_entity, output) = Entity.set_production city_entity in
      let _ = a := Entity.City city_entity in
      if output = None then cycle_cities b player
      else
        let new_unit_type = (
          match output with
          | Some u -> u
          | _ -> failwith "Error: expected new Unit to be produced"
        ) in
        let new_entity = ref (Entity.new_unit new_unit_type (Entity.tile !a)) in
        let player =
          { player with
            entities = new_entity::player.entities;
            num_units = player.num_units + 1
          } in
        cycle_cities b player in
  cycle_cities city_refs p

let set_growth p =
  let city_refs = filter_city_refs p in
  let rec cycle_cities refs =
    match refs with
    | [] -> p
    | a::b ->
      let city_entity = (
        match !a with
        | Entity.City c -> c
        | _ -> failwith "Error: expected City but got Unit"
      ) in
      let city_entity = Entity.set_growth city_entity in
      let _ = a := Entity.City city_entity in
      cycle_cities b in
  cycle_cities city_refs

let research_tech player tech =
  { player with current_tech = Some tech }

let set_new_city player tile =
  let is_capital = player.num_cities = 0 in
  { player with num_cities = player.num_cities + 1;
          entities = ref (Entity.new_city tile is_capital) :: player.entities }

let available_improvements player =
  let helper acc i =
    match i with
    | None -> acc
    | Some i -> i :: acc in
  let lst = List.map Tech.improvements_for_tech player.techs in
  List.fold_left helper [] lst