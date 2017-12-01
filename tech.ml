type tech = {
  id: string;
}

module Tech = struct
  type t = tech
  let compare t1 t2 = String.compare t1.id t2.id
end

module TechSet = Set.Make(Tech)
