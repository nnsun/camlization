open Primitives

type tech =
  | Archery
  | Pottery
  | Mining
  | Masonry

type tech_info = {
  cost : int;
  prereqs : tech list
}

let tech_info_map = [
  Archery, { cost = 0; prereqs = [] };
  Pottery, { cost = 0; prereqs = [] };
  Mining, { cost = 0; prereqs = [] };
  Masonry, { cost = 0; prereqs = [] };
]

let tech_cost tech = (List.assoc tech tech_info_map).cost

let prereqs tech = (List.assoc tech tech_info_map).prereqs