open Primitives

type player = {
  gold: int;
  science: science * science; (* Current x Needed *)
  techs: Tech.TechSet.t
}

let new_player = {
  gold = 0;
  science = (0, 300);
  techs = Tech.TechSet.empty
}

let gold p = p.gold

let gold_rate p =
  failwith "Unimplemented"

let points p =
  failwith "Unimplemented"

let science p =
  failwith "Unimplemented"

let science_rate p =
  failwith "Unimplemented"

let techs p =
  failwith "Unimplemented"
