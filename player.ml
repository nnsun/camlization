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

let gold_rate p = 0

let points p = 100

let science p = p.science

let science_rate p = 0

let techs p = p.techs

let entities p = p.entities
