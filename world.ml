type terrain = Grassland | Plains | Desert | Tundra | Ice
  | Ocean | Coast | Lake

type feature = Forest | Jungle | Oasis | FloodPlains

type elevation = Flatland | Hill | Peak

type resource =
  | Fish | Crab
  | Gold | Silver | Gems | Salt | Iron
  | Marble | Stone
  | Wheat | Corn | Rice
  | Furs | Ivory | Deer
  | Sheep | Cattle | Horses
  | Cotton | Banana | Sugar

type improvement = FishingBoats | Mine | Quarry | Farm | Camp | Pasture | Plantation

type tile_yields = {
  gold : int;
  food : int;
  production : int
}

let terrain_yields_map = [
  Grassland, { food = 2; production = 0; gold = 0 };
  Plains, { food = 1; production = 1; gold = 0 };
  Desert, { food = 0; production = 0; gold = 0 };
  Tundra, { food = 1; production = 0; gold = 0 };
  Ice, { food = 0; production = 0; gold = 0 };
  Ocean, { food = 1; production = 0; gold = 1 };
  Coast, { food = 1; production = 0; gold = 2 };
  Lake, { food = 2; production = 0; gold = 2 };
]

let feature_yields_map = [
  Forest, { food = 0; production = 1; gold = 0; };
  Jungle, { food = -1; production = 0; gold = 0 };
  Oasis, { food = 3; production = 0; gold = 2 };
  FloodPlains, { food = 3; production = 0; gold = 1 };
]


let elevation_yields_map = [
  Flatland, { food = 0; production = 0; gold = 0 };
  Hill, { food = -1; production = 1; gold = 0 };
  Peak, { food = -1000; production = -1000; gold = -1000 };
]

let resource_yields_map = [
  Fish, { food = 1; production = 0; gold = 0 };
  Crab, { food = 1; production = 0; gold = 0 };
  Gold, { food = 0; production = 0; gold = 2 };
  Silver, { food = 0; production = 0; gold = 2 };
  Gems, { food = 0; production = 0; gold = 2 };
  Salt, { food = 1; production = 0; gold = 1 };
  Iron, { food = 0; production = 1; gold = 0 };
  Marble, { food = 0; production = 1; gold = 1 };
  Stone, { food = 0; production = 1; gold = 0 };
  Wheat, { food = 1; production = 0; gold = 0 };
  Corn, { food = 1; production = 0; gold = 0 };
  Rice, { food = 1; production = 0; gold = 0 };
  Furs, { food = 0; production = 0; gold = 2 };
  Ivory, { food = 0; production = 0; gold = 2 };
  Deer, { food = 1; production = 0; gold = 0 };
  Sheep, { food = 1; production = 0; gold = 0 };
  Cattle, { food = 1; production = 0; gold = 0 };
  Horses, { food = 0; production = 1; gold = 0 };
  Cotton, { food = 0; production = 0; gold = 2 };
  Banana, { food = 1; production = 0; gold = 0 };
  Sugar, { food = 0; production = 0; gold = 2 };
]

let improvement_yields_map res = [
  FishingBoats, { food = 2; production = 0; gold = 0 };
  Mine, { food = 0; production = 1; gold = 0 };
  Quarry, { food = 0; production = 1; gold = 0 };
  Farm, { food = 1; production = 0; gold = 0 };
  Camp, { food = 0; production = 0; gold = 1 };
  (
    match res with
    | None -> Pasture, { food = 0; production = 0; gold = 0 }
    | Some r ->
      if r = Horses then Pasture, { food = 0; production = 1; gold = 0 }
      else Pasture, { food = 1; production = 0; gold = 0 }
  );
  (
    match res with
    | None -> Plantation, { food = 0; production = 0; gold = 0 }
    | Some r ->
      if r = Banana then Plantation, { food = 1; production = 0; gold = 0 }
      else Plantation, { food = 0; production = 0; gold = 1 }
  );
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
    | Some improv ->
      (List.assoc improv (improvement_yields_map tile.resource)).food in
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
    | Some improv ->
      (List.assoc improv (improvement_yields_map tile.resource)).production in
  if yield < 0 then 0 else yield

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
      | Some improv ->
        (List.assoc improv (improvement_yields_map tile.resource)).gold in
    if yield < 0 then 0 else yield

let improvement tile = tile.improvement

