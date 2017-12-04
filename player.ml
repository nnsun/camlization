open Primitives

type player = {
  gold: int;
  current_tech : Tech.tech option;
  science: science;
  techs: Tech.tech list;
  entities : Entity.entity list;
  num_cities : int;
  num_units : int;
}

let new_player = {
  gold = 0;
  current_tech = None;
  science = 0;
  techs = [];
  entities = [];
  num_cities = 0;
  num_units = 0;
}

let gold p = p.gold

let gold_rate p =
  let cities = List.filter (Entity.is_city) p.entities in
  let add_gold acc city =
    Entity.gold_per_turn (Entity.get_city_entity city) + acc in
  List.fold_left add_gold 0 cities

let maintenance p =
  let free_cities = 4 in
  let free_units = 10 in
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
  let cities = List.filter (Entity.is_city) p.entities in
  let add_science acc city =
    Entity.science_per_turn (Entity.get_city_entity city) + acc in
  List.fold_left add_science 0 cities

let techs p = p.techs

let entities p = p.entities

let set_gold p =
  { p with gold = p.gold + gold_rate p - maintenance p }
