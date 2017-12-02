open Primitives

type player = {
  gold: int;
  science: science * science; (* Current x Needed *)
  techs: Tech.tech list
}

let new_player = {
  gold = 0;
  science = (0, 300);
  techs = []
}

let gold p = p.gold

let gold_rate p = 69

let points p = 100

let science p = fst p.science

let science_rate p = 69

let techs p = p.techs
