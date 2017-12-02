type terrain = Grassland | Plains | Desert | Tundra | Ice
  | Ocean | Coast | Lake

type feature = Forest | Jungle | Oasis | FloodPlains

type elevation = Flatland | Hill | Peak

type resource = Horses | Iron

type improvement = Farm | Mine | Pasture

type tile_yields = {
  gold : int;
  food : int;
  production : int
}

let terrain_yields_map = [
  Grassland, { gold = 0; food = 2; production = 0 };
  Plains, { gold = 0; food = 1; production = 1 };
  Desert, { gold = 0; food = 0; production = 0 };
  Tundra, { gold = 0; food = 1; production = 0 };
  Ice, { gold = 0; food = 0; production = 0 };
  Ocean, { gold = 1; food = 1; production = 0 };
  Coast, { gold = 2; food = 1; production = 0 };
  Lake, { gold = 2; food = 2; production = 0 };
]

let feature_yields_map = [
  Forest, { gold = 0; food = 0; production = 1 };
  Jungle, { gold = 0; food = -1; production = 0 };
  Oasis, { gold = 2; food = 3; production = 0 };
  FloodPlains, { gold = 1; food = 3; production = 0 };
]


let elevation_yields_map = [
  Flatland, { gold = 0; food = 0; production = 0 };
  Hill, { gold = 0; food = -1; production = 1 };
  Peak, { gold = -1000; food = -1000; production = -1000 };
]

let resource_yields_map = [
  Horses, { gold = 0; food = 0; production = 0 };
  Iron, { gold = 0; food = 0; production = 0 };
]

let improvement_yields_map = [
  Farm, { gold = 0; food = 0; production = 0 };
  Mine, { gold = 0; food = 0; production = 0 };
  Pasture, { gold = 0; food = 0; production = 0 };
]


type tile = {
  resource : resource option;
  improvement : improvement option;
  terrain : terrain;
  feature : feature option;
  elevation : elevation
}

type map = tile array array

let generate_map =
  [||]

let get_tile m x y =
  failwith "Unimplemented"

let map_dimensions map =
  (*(Array.length (t.(0)), Array.length t)*)
  (50, 50)

let terrain tile = tile.terrain

let feature tile = tile.feature

let resource tile = tile.resource

let gold_gen tile =
  let yield =
    (List.assoc tile.terrain terrain_yields_map).gold +
    (List.assoc tile.elevation elevation_yields_map).gold +
    match tile.resource with
    | None -> 0
    | Some res -> (List.assoc res resource_yields_map).gold +
    match tile.feature with
    | None -> 0
    | Some feat -> (List.assoc feat feature_yields_map).gold +
    match tile.improvement with
    | None -> 0
    | Some improv -> (List.assoc improv improvement_yields_map).gold in
  if yield < 0 then 0 else yield

let food_gen tile =
  let yield =
    (List.assoc tile.terrain terrain_yields_map).food +
    (List.assoc tile.elevation elevation_yields_map).food +
    match tile.resource with
    | None -> 0
    | Some res -> (List.assoc res resource_yields_map).food +
    match tile.feature with
    | None -> 0
    | Some feat -> (List.assoc feat feature_yields_map).food +
    match tile.improvement with
    | None -> 0
    | Some improv -> (List.assoc improv improvement_yields_map).food in
  if yield < 0 then 0 else yield



let production_gen tile =
  let yield =
    (List.assoc tile.terrain terrain_yields_map).production +
    (List.assoc tile.elevation elevation_yields_map).production +
    match tile.resource with
    | None -> 0
    | Some res -> (List.assoc res resource_yields_map).production +
    match tile.feature with
    | None -> 0
    | Some feat -> (List.assoc feat feature_yields_map).production +
    match tile.improvement with
    | None -> 0
    | Some improv -> (List.assoc improv improvement_yields_map).production in
  if yield < 0 then 0 else yield

let improvement tile = tile.improvement

