type map = tile array array

and tile = {
  coordinates : int * int;
  resource : resource option;
  improvement : improvement option;
  terrain : terrain;
  feature : feature option;
  elevation : elevation;
  movement_cost : int
}

and terrain = Grassland | Plains | Desert | Tundra | Ice
  | Ocean | Coast | Lake

and feature = Forest | Jungle | Oasis

and elevation = Flatland | Hill | Peak

and resource =
  | Fish | Crab
  | Gold | Silver | Gems | Salt | Iron
  | Marble | Stone
  | Wheat | Corn | Rice
  | Furs | Ivory | Deer
  | Sheep | Cattle | Horses
  | Cotton | Banana | Sugar

and improvement = FishingBoats | Mine | Quarry | Farm | Camp | Pasture | Plantation

type tile_yields = {
  gold : int;
  food : int;
  production : int
}

(* Constants for food, production, and gold calculations *)
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

(* Constants for movement calculations *)
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

let get_tile m col row =
  m.(col).(row)

let map_dimensions m =
  (Array.length m, Array.length (m.(0)))

let coordinates tile = tile.coordinates

(* Helper functions for random map generation *)
let random_arr =
  let _ = Random.self_init () in
  let rows = Array.make 512 0 in
  let p = Array.map (fun _ -> Random.int 255) rows in
  let _ = Array.blit p 0 p 256 256 in p

let fade t = t *. t *. t *. (t *. (t *. 6. -. 15.) +. 10.)

let inc n = (n + 1) mod 255

let grad hash x y =
  let hash = hash mod 4 in
  if hash = 0 then x +. y
  else if hash = 1 then x -. y
  else if hash = 2 then -.x +. y
  else -.x -. y

let lerp a b x = a +. (b -. a) *. x

let perlin x y =
  let p = random_arr in
  let xi = (int_of_float x) mod 255 in
  let yi = (int_of_float y) mod 255 in
  let xf = x -. (float_of_int (int_of_float x)) in
  let yf = y -. (float_of_int (int_of_float y)) in
  let u = fade(xf) in
  let v = fade(yf) in
  let aa = p.(p.(xi) + yi) in
  let ab = p.(p.(xi) + inc yi) in
  let ba = p.(p.(inc xi) + yi) in
  let bb = p.(p.(inc xi) + inc yi) in
  let x1 = lerp (grad aa xf yf) (grad ba (xf -. 1.) yf) u in
  let x2 = lerp (grad ab xf (yf -. 1.)) (grad bb (xf -. 1.) (yf -. 1.)) u in
  (lerp x1 x2 v +. 1.) /. 2.

let map_perlin_array (matrix: tile array array) (f: int -> tile -> tile) =
  Array.mapi
    (fun i1 col -> Array.mapi
      (fun i2 row ->
        let v = int_of_float
          (100. *. (perlin
            ((float_of_int i1) /. 10.) ((float_of_int i2) /. 10.))) in
        f v matrix.(i1).(i2)) col) matrix

let generate_map =
  let base_tile = {
    coordinates = (0, 0);
    resource = None;
    improvement = None;
    terrain = Grassland;
    feature = None;
    elevation = Flatland;
    movement_cost = 1
  } in
  let matrix = Array.make_matrix 40 25 base_tile in
  let matrix = Array.mapi
    (fun i1 col -> Array.mapi
      (fun i2 row ->
        { matrix.(i1).(i2) with coordinates = (i1, i2)}) col
  ) matrix in
  let land_water n = if n >= 60 then Ocean else Grassland in
  let elevation tile n =
    if tile.terrain = Ocean then Flatland else
      if n >= 58 then Peak else if n >= 40 then Hill else Flatland in
  let trees tile n =
    if tile.terrain = Ocean || tile.elevation = Peak then None else
    if n < 40 || n > 60 then None else
      let (_, row_num) = tile.coordinates in
      let (_, rows) = map_dimensions matrix in
      let dist =
        min (abs (row_num - rows / 2)) (abs (row_num - (rows / 2 - 1))) in
      let multi = (1. -. (float_of_int dist) /. (float_of_int rows)) ** 3. in
      let v = multi *. (float_of_int (abs (n - 50))) in
      if v < 1. then None
      else if v < 6. then Some Forest
      else Some Jungle in
  let deserts tile n =
    if tile.terrain = Ocean || tile.elevation = Peak ||
        n < 40 || n > 60 then tile.terrain else
    let (_, row_num) = tile.coordinates in
    let (_, rows) = map_dimensions matrix in
    let dist =
      min (abs (row_num - rows / 2)) (abs (row_num - (rows / 2 - 1))) in
    let multi = (1. -. ((float_of_int dist) /. (float_of_int rows))) ** 3. in
    let v = multi *. (float_of_int (abs (n - 50))) in
    if v > 5. then Desert else tile.terrain in
  let ice_tundra tile n =
    let (_, row_num) = tile.coordinates in
    let (_, rows) = map_dimensions matrix in
    let dist =
      min (abs (row_num - rows / 2)) (abs (row_num - (rows / 2 - 1))) in
    let multi =
      (((float_of_int dist) +. (float_of_int rows /. 2.))
          /. (float_of_int rows)) ** 10. in
    let v = multi *. (float_of_int (abs (n - 50))) in
    if v > 8. then
      if tile.terrain = Ocean then Ice else Tundra
    else tile.terrain in
  let matrix = map_perlin_array matrix
      (fun v t -> { t with terrain = land_water v }) in
  let matrix = map_perlin_array matrix
      (fun v t -> { t with elevation = (elevation t v) }) in
  let matrix = map_perlin_array matrix
      (fun v t -> { t with feature = (trees t v) }) in
  let matrix = map_perlin_array matrix
      (fun v t -> { t with terrain = deserts t v; feature = None }) in
  let matrix = map_perlin_array matrix
      (fun v t -> { t with terrain = ice_tundra t v }) in
  matrix

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
  abs(x2 - x1) <= 1 && abs(y2 - y1) <= 1

let adjacent_tiles tile map =
  let (x, y) = tile.coordinates in
  let unfiltered = [
    (x - 1, y);
    (x + 1, y);
    (x - 1, y + 1);
    (x - 1, y - 1);
    (x + 1, y + 1);
    (x + 1, y - 1);
  ] in
  let pred map t =
    let (num_cols, num_rows) = map_dimensions map in
    let (x, y) = t in
    if x < 0 || x >= num_cols || y < 0 || y >= num_rows then false else true in
  List.map (fun (a, b) -> get_tile map a b) (List.filter (pred map) unfiltered)

let set_improvement map col row improvement =
  let tile = get_tile map col row in
  let feature = tile.feature in
  let new_feature = if feature = Some Forest || feature = Some Jungle
    then None else feature in
  map.(col).(row) <- {tile with improvement = Some improvement; feature = new_feature}

let tile_possible_improvements tile =
  let improvement_for_resource r =
    match r with
    | Fish | Crab -> FishingBoats
    | Gold | Silver | Gems | Salt | Iron -> Mine
    | Marble | Stone -> Quarry
    | Wheat | Corn | Rice -> Farm
    | Furs | Ivory | Deer -> Camp
    | Sheep | Cattle | Horses -> Pasture
    | Cotton | Banana | Sugar -> Plantation in
  let i_list = match resource tile with
    | None -> []
    | Some r ->
      let i = improvement_for_resource r in
      if tile.improvement <> Some i then [i] else [] in
  let i_list2 =
    if i_list <> [Farm] && tile.elevation = Flatland &&
      (tile.terrain = Grassland || tile.terrain = Plains) && tile.improvement <> Some Farm then
      Farm :: i_list
    else i_list in
  if i_list <> [Mine] && tile.elevation = Hill && tile.improvement <> Some Mine then
    Mine :: i_list
  else i_list2

let defense_multiplier tile =
  let feature_bonus =
    match tile.feature with
    | Some Forest | Some Jungle -> 0.5
    | _ -> 0. in
  feature_bonus +. (if tile.elevation = Hill then 1.25 else 1.)