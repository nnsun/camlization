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

let feature_movement_cost feat =
  match feat with
  | Some Forest -> 1
  | Some Jungle -> 1
  | _ -> 0

let elevation_movement_cost elev =
  match elev with
  | Flatland -> 0
  | Hill -> 1
  | Peak -> 1000

type tile = {
  coordinates : int * int;
  resource : resource option;
  improvement : improvement option;
  terrain : terrain;
  feature : feature option;
  elevation : elevation;
  movement_cost : int
}

let sample_tile = {
  coordinates = (0, 0);
  resource = Some Wheat;
  improvement = Some Farm;
  terrain = Grassland;
  feature = Some Forest;
  elevation = Peak;
  movement_cost = 1
}

let sample_tile2 = {
  coordinates = (1, 0);
  resource = Some Cattle;
  improvement = Some Farm;
  terrain = Coast;
  feature = None;
  elevation = Hill;
  movement_cost = 1
}

let sample_tile3 = {
  coordinates = (2, 0);
  resource = Some Wheat;
  improvement = Some Farm;
  terrain = Grassland;
  feature = Some Forest;
  elevation = Flatland;
  movement_cost = 1
}

let sample_tile4 = {
  coordinates = (3, 0);
  resource = None;
  improvement = None;
  terrain = Lake;
  feature = None;
  elevation = Peak;
  movement_cost = 1
}

let sample_tile5 = {
  coordinates = (4, 0);
  resource = Some Corn;
  improvement = Some Mine;
  terrain = Plains;
  feature = None;
  elevation = Flatland;
  movement_cost = 1
}

type map = tile array array

let generate_map =
  let arr = [|sample_tile; sample_tile2; sample_tile3; sample_tile4; sample_tile5|] in
  let arr2 = [|sample_tile5; sample_tile4; sample_tile3; sample_tile2; sample_tile|] in
  let row = Array.concat [arr; arr2; arr; arr2; arr; arr2; arr; arr2; arr; arr2] in
  Array.make 50 row

let get_tile m col row =
  m.(row).(col)

let map_dimensions m =
  (Array.length (m.(0)), Array.length m)

let coordinates tile = tile.coordinates

let terrain tile = tile.terrain

let feature tile = tile.feature

let elevation tile = tile.elevation

let resource tile = tile.resource

let is_strategic resource =
  match resource with
  | Horses -> true
  | Iron -> true
  | _ -> false

let improvement tile = tile.improvement

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

let tile_yields tile = {
  food = food_gen tile;
  production = production_gen tile;
  gold = gold_gen tile
}

let movement_cost tile = tile.movement_cost

let is_adjacent tile1 tile2 =
  let (x1, y1) = tile1.coordinates in
  let (x2, y2) = tile2.coordinates in
  if abs(x2 - x1) <= 1 && abs(y2 - y1) <= 1 then true else false

let adjacent_tiles tile map =
  let (x, y) = tile.coordinates in
  let unfiltered = [
    tile;
    get_tile map (x - 1) y;
    get_tile map (x + 1) y;
    get_tile map (x - 1) (y + 1);
    get_tile map (x - 1) (y - 1);
    get_tile map (x + 1) (y + 1);
    get_tile map (x + 1) (y - 1);
  ] in
  let pred map t =
    let (num_cols, num_rows) = map_dimensions map in
    let (x, y) = t.coordinates in
    if x < 0 || x >= num_cols || y < 0 || y >= num_rows then false else true in
  List.filter (pred map) unfiltered