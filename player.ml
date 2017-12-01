
type player = {
  gold: int;
  science: int * int; (* Current x Needed *)
  techs: Tech.TechSet.t;
  entities: Entity.entity ref array;
}