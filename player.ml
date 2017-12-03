open Primitives

type player = {
  gold: int;
  current_tech : Tech.tech option;
  science: science;
  techs: Tech.tech list;
  entities : Entity.entity list
}

let new_player = {
  gold = 0;
  current_tech = None;
  science = 0;
  techs = [];
  entities = [];
}

let gold p = p.gold

let gold_rate p =
  let cities = List.filter (Entity.is_city) p.entities in
  let add_gold acc city =
    Entity.gold_per_turn (Entity.get_city_entity city) + acc in
  List.fold_left add_gold 0 cities

let points p = 100

let science p = p.science

let science_rate p =
  let cities = List.filter (Entity.is_city) p.entities in
  let add_science acc city =
    Entity.science_per_turn (Entity.get_city_entity city) + acc in
  List.fold_left add_science 0 cities

let techs p = p.techs

let entities p = p.entities
