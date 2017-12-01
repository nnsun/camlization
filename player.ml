
type player = {
  gold: int;
  science: int * int; (* Current x Needed *)
  techs: Tech.TechSet.t
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
