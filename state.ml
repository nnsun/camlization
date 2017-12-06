open Primitives

type multiplayer_options = {
  player_count: int
}

type menu_state =
  | Loading
  | Copyright
  | Main of int
  | Multiplayer of int * multiplayer_options
  | Options
  | About

type pane_state =
  | Tile
  | City of int
  | Unit of int * int
  | Tech of int

type game_state = {
  player_turns: int;
  map: World.map;
  map_display: int * int;
  selected_tile: int * int;
  pane_state: pane_state;
  current_player: int;
  players: Player.player array;
}

type state =
  | Menu of menu_state
  | Game of game_state
  | Quit

let start_state = Menu (Loading)

let initial_game_state options =
  {
    player_turns = 0;
    map = World.generate_map;
    map_display = (0, 0);
    selected_tile = (0, 0);
    pane_state = Tile;
    current_player = 0;
    players = Array.append (Array.make (options.player_count - 1) Player.new_player) [| Player.new_player2 |]
  }

let turns gst = gst.player_turns / Array.length gst.players

let date gst =
  let date = initial_year + (turns gst * years_per_turn) in
  let suffix = if date < 0 then " BCE" else " CE" in
  string_of_int (abs date) ^ suffix

let game_map gst = gst.map

let entities_refs coordinates gst =
  let valid_entity e =
    let entity = !e in
    Entity.health entity > 0
    && (World.coordinates (Entity.tile entity) = coordinates)
  in
  let entities_of_player p =
    List.map (fun e -> p, e) (List.filter valid_entity (Player.entities p)) in
  let players = Array.map entities_of_player gst.players in
  players |> Array.to_list |> List.flatten

let entities coordinates gst =
  entities_refs coordinates gst |> List.map (fun (p, e) -> p, !e)

let units coordinates gst =
  let l = entities coordinates gst in
  let map (player, unit) =
    match unit with
    | Entity.Unit u -> [(player, u)]
    | _ -> []
  in
  List.map map l |> List.flatten

let unit_refs coordinates gst =
  let l = entities_refs coordinates gst in
  List.filter (fun (p, e) -> Entity.is_unit !e) l

let city coordinates gst =
  let is_city (p, e) = Entity.is_city e in
  let l = List.filter is_city (entities coordinates gst) in
  try
    match List.hd l with
    | (p, Entity.City c) -> Some (p, c)
    | _ -> None
  with _ -> None

let city_ref coordinates gst =
  let is_city (p, e) = Entity.is_city !e in
  let l = List.filter is_city (entities_refs coordinates gst) in
  try
    Some (List.hd l)
  with _ -> None

let rec satisfies_pred pred lst =
  match lst with
  | [] -> None
  | a::b ->
    if pred a then Some a else satisfies_pred pred b

let tile_contains_enemy state tile =
  let player = state.players.(state.current_player) in
  let rec cycle_players players =
    match players with
    | [] -> None
    | a::b ->
      if a = player then cycle_players b
      else
        let entities_ref_list = Player.entities a in
        let is_on_tile t entity_ref =
          if Entity.health (!entity_ref) <= 0 then false else
            World.coordinates (Entity.tile !entity_ref) = World.coordinates t in
        let sat_pred_result = satisfies_pred (is_on_tile tile) entities_ref_list in
        if sat_pred_result <> None then sat_pred_result
        else cycle_players b in
  cycle_players (Array.to_list state.players)

let combat e1 e2 =
  let _ = Random.self_init () in
  let tile = Entity.tile e2 in
  let rec combat_round e1 e2 =
    let e1_str = Entity.relative_str e1 e2 in
    let e2_str = World.(Entity.relative_str e2 e1) in
    let e2_str = (World.defense_multiplier tile) *. e2_str in
    if e1_str = 0. || e2_str = 0. then (e1, e2)
    else
      let rand_float = Random.float 1. in
      let health_drop =
        if rand_float <= e1_str /. (e1_str +. e2_str) then
          int_of_float (20. *. (3. *. e1_str +. e2_str) /.
                        (3. *. e2_str +. e1_str))
        else 0 in
      let new_e2 = Entity.set_health e2 (Entity.health e2 - health_drop) in
      if Entity.health new_e2 <= 0 then (e1, new_e2)
      else
        let e2_str = World.(Entity.relative_str new_e2 e1) in
        let e2_str = (World.defense_multiplier tile) *. e2_str in
        let rand_float = Random.float 1. in
        let health_drop =
          if rand_float <= e2_str /. (e1_str +. e2_str) then
            int_of_float (20. *. (3. *. e2_str +. e1_str) /.
                          (3. *. e1_str +. e2_str))
          else 0 in
          let new_e1 = Entity.set_health e1 (Entity.health e1 - health_drop) in
          if Entity.health new_e1 <= 0 then (new_e1, new_e2)
          else combat_round new_e1 new_e2 in
  combat_round e1 e2


let make_move state entity_ref tile =
  let player = state.players.(state.current_player) in
  if not (List.mem entity_ref (Player.entities player)) then (state, false)
  else
    let unit_entity = Entity.get_unit_entity !entity_ref in
    let moves_left = Entity.moves_left unit_entity in
    if moves_left <= 0 then (state, false) else
    let cost = World.movement_cost tile in
    let update_tile unit_entity tile =
      Entity.set_tile unit_entity tile in
    let go_to_tile st e_ref t_ref =
      let opponent_opt = tile_contains_enemy st t_ref in
      match opponent_opt with
      | None ->
        let unit_entity =
          Entity.subtract_moves_left unit_entity cost in
        let unit_entity =
          Entity.set_tile unit_entity tile in
        let _ = entity_ref := Entity.Unit unit_entity in
        (state, true)
      | Some o ->
        if Entity.unit_class
                (Entity.unit_type unit_entity) = Entity.Civilian then
          (state, false)
        else
          let (new_e1, new_e2) = combat (Entity.Unit unit_entity) (!o) in
          let unit_entity = (
            match new_e1 with
            | Entity.Unit u -> u
            | _ -> failwith "Error: expected Unit but got City"
          ) in
          let _ = entity_ref := Entity.Unit unit_entity in
          let _ = o := new_e2 in
          if tile_contains_enemy state tile = None then
            let unit_entity = update_tile unit_entity tile in
            let _ = entity_ref := Entity.Unit unit_entity in
            (state, true)
          else (state, false) in

    let utype = Entity.unit_type unit_entity in
    let unit_tile = Entity.tile (Entity.Unit unit_entity) in
    let terrain = World.terrain tile in
    if not (World.is_adjacent unit_tile tile) then (state, false) else
    if World.elevation tile = World.Peak then (state, false)
    else if terrain = World.Ice then (state, false)
    else if utype <> Entity.Trireme && utype <> Entity.WorkBoat &&
          (terrain = World.Ocean || terrain = World.Coast) then
      let techs = Player.techs player in
      let is_optics tech = tech = Tech.Optics in
      if not (List.exists is_optics techs) then (state, false)
      else go_to_tile state unit_entity tile
    else if (utype = Entity.Trireme || utype = Entity.WorkBoat) &&
            (terrain <> World.Ocean && terrain <> World.Coast) then (state, false)
    else go_to_tile state unit_entity tile

let strategics state =
  let p = state.players.(state.current_player) in
  let city_refs = Player.filter_city_refs p in
  let rec cycle_cities acc refs =
    match refs with
    | [] -> acc
    | a::b ->
      let city_tile = Entity.tile !a in
      let adj_tiles = World.adjacent_tiles city_tile state.map in
      let acc =
        if World.resource city_tile = Some World.Horses then
          World.Horses::acc
        else if World.resource city_tile = Some World.Iron then
          World.Iron::acc
        else acc in
      let rec check_tiles inner_acc tiles =
        match tiles with
        | [] -> inner_acc
        | c::d ->
          if World.resource c = Some World.Horses
              && World.improvement c = Some World.Pasture then
            check_tiles (World.Horses::inner_acc) d
          else if World.resource c = Some World.Iron
              && World.improvement c = Some World.Mine then
            check_tiles (World.Iron::inner_acc) d
          else check_tiles inner_acc d in
      cycle_cities (List.rev_append acc (check_tiles [] adj_tiles)) b in
  cycle_cities [] city_refs

let available_techs state =
  let p = state.players.(state.current_player) in
  let researched = Player.techs p in
  let rec cycle_techs acc lst =
    match lst with
    | [] -> acc
    | a::b ->
      let prereqs = Tech.prereqs a in
      if not (List.mem a researched) &&
          List.for_all (fun t -> List.mem t researched) prereqs then
        cycle_techs (a::acc) b
      else cycle_techs acc b in
  cycle_techs [] Tech.tech_list

let available_units state =
  let p = state.players.(state.current_player) in
  let researched = Player.techs p in
  let units = Entity.units_list in
  let pred u =
    match Entity.tech_req u with
    | None -> true
    | Some t -> List.mem t researched in
  let units = List.filter pred units in
  let pred u =
    match Entity.resource_req u with
    | None -> true
    | Some res -> List.mem res (strategics state) in
  List.filter pred units


let worked_tiles city map =
  let workable_tiles = World.adjacent_tiles (Entity.tile city) map in
  let sorted_tiles = List.rev
    (List.sort (fun a b ->
        World.food_gen a + World.production_gen a + World.gold_gen a -
        World.food_gen b - World.production_gen b - World.gold_gen b)
    workable_tiles) in
  let pop = Entity.population (Entity.get_city_entity city) in
  let rec take n acc lst =
    if n <= 0 then acc
    else
      match lst with
      | [] -> acc
      | a::b ->
        take (n - 1) (a::acc) b in
  take pop [] sorted_tiles

let set_city_yields state =
  let p = state.players.(state.current_player) in
  let city_entities = Player.filter_city_refs p in
  let rec cycle_cities cities =
    match cities with
    | [] -> ()
    | a::b ->
      let worked = worked_tiles !a state.map in
      let city_tile = Entity.tile !a in
      let worked = city_tile::worked in
      let food =
        List.fold_left (fun g t -> g + World.food_gen t) 0 worked
            + (if Entity.is_capital (Entity.get_city_entity !a) then 4 else 0) in
      let production =
        List.fold_left (fun g t -> g + World.production_gen t) 0 worked
            + (if Entity.is_capital (Entity.get_city_entity !a) then 4 else 0) in
      let gold =
        List.fold_left (fun g t -> g + World.gold_gen t) 0 worked
            + (if Entity.is_capital (Entity.get_city_entity !a) then 4 else 0) in
      let new_entity = !a |> (Entity.set_food_per_turn food) |>
                            (Entity.set_production_per_turn production) |>
                            (Entity.set_gold_per_turn gold) in
      let _ = a := new_entity in
      cycle_cities b in
  cycle_cities city_entities

let next_turn state =
  let player = state.players.(state.current_player) in
  let player = Player.set_gold player in
  let player = Player.set_science player in
  let player = Player.set_production player in
  let player = Player.set_growth player in
  let _ = set_city_yields state in
  let valid_entity e =
    let entity = !e in
    Entity.health entity > 0 in
  let update_movements_and_health p =
    let entities_of_player = List.filter valid_entity (Player.entities p) in
    let units_of_player = List.filter (fun e -> Entity.is_unit !e) entities_of_player in
    List.iter (fun e -> 
      let u = Entity.get_unit_entity (!e) in
      if Entity.moves_left u = Entity.movement_points (Entity.unit_type u) then 
        let health = Entity.health (!e) in e := Entity.set_health (!e) (health + 10) else ();
        e := Entity.Unit (Entity.reset_movement u)) units_of_player in
    let () = if state.current_player + 1 = Array.length state.players then
      Array.iter update_movements_and_health state.players in
  state.players.(state.current_player) <- player;
  {
    state with
    current_player = (state.current_player + 1) mod (Array.length state.players);
    player_turns = state.player_turns + 1
  }

let found_city gst tile worker_unit =
  let (col, row) = World.coordinates tile in
  let entity_refs = List.map (fun p -> Player.entities p) (gst.players |> Array.to_list) |> List.flatten in
  let entities = List.map (!) entity_refs in
  let cities = List.filter Entity.is_city entities in
  let filter_close_cities c =
    let t = Entity.tile c in
    let (c_col, c_row) = World.coordinates t in
    if c_col mod 2 = 1 then
      (col, row) = (c_col, c_row) ||
      (col, row) = (c_col - 1, c_row) ||
      (col, row) = (c_col, c_row - 1) ||
      (col, row) = (c_col + 1, c_row) ||
      (col, row) = (c_col + 1, c_row + 1) ||
      (col, row) = (c_col, c_row + 1) ||
      (col, row) = (c_col - 1, c_row + 1) ||
      (col, row) = (c_col - 2, c_row - 1) ||
      (col, row) = (c_col - 3, c_row - 1) ||
      (col, row) = (c_col, c_row - 2) ||
      (col, row) = (c_col, c_row - 3) ||
      (col, row) = (c_col + 2, c_row - 1) ||
      (col, row) = (c_col + 3, c_row - 1) ||
      (col, row) = (c_col - 2, c_row + 1) ||
      (col, row) = (c_col - 3, c_row + 1) ||
      (col, row) = (c_col, c_row + 2) ||
      (col, row) = (c_col, c_row + 3) ||
      (col, row) = (c_col + 2, c_row + 1) ||
      (col, row) = (c_col + 3, c_row + 2) ||
      (col, row) = (c_col - 1, c_row + 2) ||
      (col, row) = (c_col + 1, c_row + 2) ||
      (col, row) = (c_col - 1, c_row - 2) ||
      (col, row) = (c_col + 1, c_row - 2)
    else
      (col, row) = (c_col, c_row) ||
      (col, row) = (c_col - 1, c_row - 1) ||
      (col, row) = (c_col, c_row - 1) ||
      (col, row) = (c_col + 1, c_row - 1) ||
      (col, row) = (c_col + 1, c_row) ||
      (col, row) = (c_col, c_row + 1) ||
      (col, row) = (c_col - 1, c_row) ||
      (col, row) = (c_col - 2, c_row - 1) ||
      (col, row) = (c_col - 3, c_row - 2) ||
      (col, row) = (c_col, c_row - 2) ||
      (col, row) = (c_col, c_row - 3) ||
      (col, row) = (c_col + 2, c_row - 1) ||
      (col, row) = (c_col + 3, c_row - 2) ||
      (col, row) = (c_col - 2, c_row + 1) ||
      (col, row) = (c_col - 3, c_row + 1) ||
      (col, row) = (c_col, c_row + 2) ||
      (col, row) = (c_col, c_row + 3) ||
      (col, row) = (c_col + 2, c_row + 1) ||
      (col, row) = (c_col + 3, c_row + 1) ||
      (col, row) = (c_col - 1, c_row + 1) ||
      (col, row) = (c_col + 1, c_row + 1) ||
      (col, row) = (c_col - 1, c_row - 2) ||
      (col, row) = (c_col + 1, c_row - 2)
    in
  if List.filter filter_close_cities cities |> List.length = 0 then
    let player = Player.found_city (gst.players.(gst.current_player)) tile in
    let _ = worker_unit := Entity.set_health !worker_unit 0 in
    gst.players.(gst.current_player) <- player; gst
  else gst

let player_number gst p =
  let rec find p i = function
    | [] -> failwith "Player not found"
    | h::t -> if p = h then i else find p (i+1) t
  in
  find p 1 (Array.to_list gst.players)