open Notty
open Notty_unix
open Notty_helper
open State

(* Constants and Helpers *)

let is_mac_os = true

let left_padding = 1
let top_padding = 1
let right_padding = 0
let bottom_padding = 4
let gui_bar_padding = 2
let tile_width = 17
let tile_height = 10
let left_pane_frac = 0.2

let pane_width w =
  (float_of_int w) *. left_pane_frac |> int_of_float

let initial_tile_left w =
  left_padding + (pane_width w)
let initial_tile_top = top_padding

let grid xxs = xxs |> List.map I.hcat |> I.vcat

let status_bar (w, h) gst =
  let player = gst.players.(gst.current_player) in
  let science_text = "+" ^ string_of_int (Player.science_rate player) in
  let gold_text = string_of_int (Player.gold player) in
  let gold_rate =
    let rate = Player.gold_rate player in
    if rate >= 0
      then I.string A.(fg yellow ++ bg black) ("(+" ^ string_of_int rate ^ ")")
    else I.string A.(fg red ++ bg black) ("(" ^ string_of_int rate ^ ")")
  in
  let status = I.hsnap ~align:`Right w (I.hcat [
    I.string A.(empty ++ bg black) ("Turns: " ^ string_of_int (State.turns gst));
    I.string A.(empty ++ bg black) (
      "  "
      ^ date gst
    );
    I.string A.(empty ++ bg black) "  QUIT "
  ]) in
  let metrics = I.hcat [
    I.void 1 1;
    I.uchar A.(fg blue ++ bg black) 9877 1 1;
    I.string A.(fg blue ++ bg black) science_text;
    I.void 1 1;
    I.uchar A.(fg yellow ++ bg black) 11044 1 1;
    I.void 1 1;
    I.string A.(fg yellow ++ bg black) gold_text;
    gold_rate
  ] in
  let background = I.tile w 1 (I.string A.(bg black) " ") in
  I.(metrics </> status </> background)

let player_bar (w, h) gst =
  let pimg i p =
    let player_string = " PLAYER " ^ string_of_int i ^ " " in
    let len = String.length player_string in
    if gst.current_player = i
    then
      I.(vcat [
        hsnap len (uchar A.(fg white) 9660 1 1);
        string A.(fg white ++ bg blue) (String.make len ' ');
        string A.(fg white ++ bg blue) player_string;
        string A.(fg white ++ bg blue) (String.make len ' ');
      ])
    else
      I.(vcat [
        void 1 1;
        string A.empty (String.make len ' ');
        string A.empty player_string;
        string A.empty (String.make len ' ');
      ])
  in
  let bar = Array.mapi pimg gst.players |> Array.to_list |> I.hcat in
  I.(pad ~l:(pane_width w) ~t:(h - 4) (hsnap (w - pane_width w) bar))

let tile_yields_img tile =
  World.(
    let y = tile_yields tile in
    I.(hcat [
      uchar A.(fg green ++ bg black) 127823 1 1;
      void 1 1;
      string A.(fg green ++ bg black) (string_of_int y.food);
      void 1 1;
      uchar A.(fg yellow ++ bg black) 11044 1 1;
      void 1 1;
      string A.(fg yellow ++ bg black) (string_of_int y.gold);
      void 1 1;
      uchar A.(fg blue ++ bg black) 128296 1 1;
      void 1 1;
      string A.(fg blue ++ bg black) (string_of_int y.production);
    ]))

let progress_str f =
  if f < 0.15 then "▁"
  else if f < 0.30 then "▂"
  else if f < 0.45 then "▃"
  else if f < 0.60 then "▅"
  else if f < 0.75 then "▆"
  else if f < 0.90 then "▇"
  else "█"

let tech_str t =
  Tech.(
    match t with
    | Agriculture -> "Agriculture"
    | Fishing -> "Fishing"
    | Sailing -> "Sailing"
    | Optics -> "Optics"
    | AnimalHusbandry -> "Animal Husbandry"
    | Trapping -> "Trapping"
    | HorsebackRiding -> "Horseback Riding"
    | Mining -> "Mining"
    | Masonry -> "Masonry"
    | Calendar -> "Calendar"
    | BronzeWorking -> "Bronze Working"
    | IronWorking -> "Iron Working"
    | Archery -> "Archery"
    | TheWheel -> "The Wheel"
    | Mathematics -> "Mathematics"
  )

let unit_str u =
  Entity.(
    match Entity.get_unit_type u with
    | Worker -> "Worker"
    | Scout -> "Scout"
    | Warrior -> "Warrior"
    | WorkBoat -> "Work Boat"
    | Archer -> "Archer"
    | Trireme -> "Trireme"
    | Spearman -> "Spearman"
    | Chariot -> "Chariot"
    | Horseman -> "Horseman"
    | Swordsman -> "Swordsman"
    | Catapult -> "Catapult"
  )

let unit_list_img ul selected_unit =
  let rec unit_list_img_helper ul selected_unit current_unit =
    let health = A.(fg red ++ bg black) in
    match ul with
    | [] -> I.void 1 1
    | u :: us ->
      let text =
        if selected_unit = current_unit
        then A.(fg white ++ bg black ++ st bold)
        else A.(fg white ++ bg black) in
      let arrow =
        if selected_unit = current_unit
        then I.(I.uchar text 9654 1 1 <|> I.void 1 1)
        else I.void 2 1 in
      I.vcat [
        I.hcat [
          arrow;
          I.string text (unit_str u);
          I.void 1 1;
          I.uchar health 9829 1 1;
          I.void 1 1;
          I.string health (string_of_int (Entity.health (Entity.Unit u)))
        ];
        unit_list_img_helper us selected_unit (current_unit+1)
      ] in
  unit_list_img_helper ul selected_unit 0

let left_pane (w, h) gst =
  let player = gst.players.(gst.current_player) in
  let pane_width = pane_width w in
  let snap img = I.hsnap pane_width img in
  let text = A.(fg white ++ bg black) in
  let underlined = A.(fg white ++ bg black ++ st underline) in
  let (col, row) = gst.selected_tile in
  let pane_content = match gst.pane_state with
  | Tile -> I.vcat [
      snap (I.(string text "TILE"));
      snap (I.(string text "City: C, Tech: T, Units: U"))
    ]
  | Unit u -> let units = State.units (col, row) gst in
    let num_units = List.length units in
    I.vcat [
      snap (I.(string A.(text ++ st underline)) "UNITS");
      snap (I.(string text "City: C, Tech: T, Tile: S"));
      I.void 1 1;
      snap (I.string text "MOVEMENT:");
      snap (I.string text "___");
      snap (I.string text "/   \\");
      snap (I.string text ",--(     )--.");
      snap (I.(hcat [
        string text "/    \\_"; string underlined "2"; string text "_/    \\"
      ]));
      snap (I.string text "\\  1 /   \\ 3  /");
      snap (I.string text ")--(     )--(");
      snap (I.string text "/  4 \\___/ 6  \\");
      snap (I.string text "\\    / 5 \\    /");
      snap (I.string text "`--(     )--'");
      snap (I.string text "\\___/");
      I.void 1 1;
      snap (I.string text "HEALTH: ");
      I.void 1 1;
      snap (I.string text "SELECT UNIT WITH .");
      I.void 1 1;
      if num_units > 0 then
        I.hsnap ~align: `Left pane_width (unit_list_img units (u mod num_units))
      else
        snap (I.string text "NO UNITS IN THIS TILE")
    ]
  | City c -> I.vcat [
      snap (I.(string A.(text ++ st underline) "CITY"));
      snap (I.(string text "Tech: T, Units: U, Tile: S"))
    ]
  | Tech t ->
    let current_tech = Player.current_tech player in
    let tech_left t =
      if Player.science_rate player = 0 then 99
      else
        (Tech.tech_cost t - Player.science player)
        / (Player.science_rate player) in
    let tech_img t =
      match current_tech with
      | Some c ->
        if t = c then
        I.string A.(fg white ++ bg black ++ st bold) (
          "▶ " ^ tech_str t ^ " (" ^ (string_of_int (tech_left t)) ^ ")"
        )
        else
        I.string text (
          "  " ^ tech_str t ^ " (" ^ (string_of_int (tech_left t)) ^ ")"
        )
      | None ->
        I.string text (
          "  " ^ tech_str t ^ " (" ^ (string_of_int (tech_left t)) ^ ")"
        )
    in
    I.vcat [
      snap (I.(string A.(text ++ st underline) "TECH"));
      snap (I.(string text "City: C, Units: U, Tile: S"));
      I.void 1 1;
      snap (I.(string text "CURRENT RESEARCH:"));
      I.void 1 1;
      (
        match current_tech with
        | Some t -> tech_img t
        | None -> snap I.(string text "Choose a Tech to Research")
      );
      I.void 1 2;
      snap (I.string text "AVAILABLE TECHS:");
      I.void 1 1;
      snap (
        I.hsnap ~align:`Left (pane_width - 8) (
          I.vcat (List.map tech_img (State.available_techs gst))
        )
      );
      I.void 1 2;
      snap (I.string text "RESEARCHED TECHS:");
      snap (I.vcat (List.map tech_img (Player.techs player)))
    ]  in
  I.zcat [
    pane_content;
    I.(char A.(fg white ++ bg black) ' ' pane_width (h - top_padding))
  ] |> I.pad ~t:top_padding

let ui_img (w, h) gst =
  I.zcat [
    status_bar (w, h) gst;
    player_bar (w, h) gst;
    left_pane (w, h) gst
  ]

let calculate_tiles_w_h (w, h) =
  let tiles_w = (float_of_int w -. float_of_int (pane_width w) -.
                float_of_int left_padding -.
                float_of_int right_padding -. (float_of_int tile_width *. 1.5)) /.
                float_of_int tile_width |> floor |> int_of_float in
  let tiles_h = (float_of_int h -. float_of_int top_padding -.
                float_of_int bottom_padding -. (float_of_int tile_height /. 2.)) /.
                float_of_int tile_height |> floor |> int_of_float in
  (tiles_w, tiles_h)

let terrain_img tile =
  World.(
    match terrain tile with
    | Grassland -> I.string A.(fg green) ",.,.,.,,.,.,.,"
    | Plains -> I.string A.(fg green) "______________"
    | Desert -> I.string A.(fg lightyellow) "____↟______↟__"
    | Tundra -> I.string A.(fg (gray 1)) "______________"
    | Ice -> I.string A.(fg lightblue) "______________"
    | Ocean -> I.string A.(fg blue) "=============="
    | Coast -> I.string A.(fg lightyellow) "=_____↟↟_____="
    | Lake -> I.string A.(fg blue) "--------------")

let feature_img f =
  World.(
    match f with
    | Forest -> I.string A.(fg green) "↟ ↟  ↟↟ ↟ ↟↟"
    | Jungle -> I.string A.(fg green) "↟↟↟↟↟↟↟↟↟↟↟↟"
    | Oasis -> I.string A.(fg blue) "↟__↟"
    | FloodPlains -> I.string A.(fg blue) "====")

let feature_opt_img tile =
  World.(
    match feature tile with
    | Some f -> feature_img f
    | None -> I.empty)

let elevation_img u tile =
  let str =
    World.(
      match elevation tile with
      | Flatland -> "            "
      | Hill -> "◠◠◠◠◠◠◠◠◠◠◠◠"
      | Peak -> "△^△^△^^△^△^△^△"
    ) in
  I.string A.(fg (if u then blue else (gray 7)) ++ st underline) str

let resource_img r = (* TODO: Make sure player has researched it *)
  World.(
    match r with
    | Fish -> I.string A.empty "Fish"
    | Crab -> I.string A.empty "Crab"
    | Gold -> I.string A.(fg yellow) "Gold"
    | Silver -> I.string A.(fg (gray 10)) "Silver"
    | Gems -> I.string A.(fg lightmagenta) "Gem"
    | Salt -> I.string A.empty "Salt"
    | Iron -> I.string A.(fg (gray 10)) "Iron"
    | Marble -> I.string A.(fg (gray 10)) "Marble"
    | Stone -> I.string A.(fg (gray 10)) "Stone"
    | Wheat -> I.string A.(fg lightyellow) "⇞⇞⇞⇞⇞⇞"
    | Corn -> I.string A.(fg yellow) "⇞⇞⇞⇞⇞⇞"
    | Rice -> I.string A.empty "⇞⇞⇞⇞⇞⇞"
    | Furs -> I.string A.empty "Furs"
    | Ivory -> I.string A.empty "Ivory"
    | Deer -> I.string A.empty "Deer"
    | Sheep -> I.string A.empty "Sheep"
    | Cattle -> I.string A.empty "Cattle"
    | Horses -> I.string A.empty "Horses"
    | Cotton -> I.string A.empty "Cotton"
    | Banana -> I.string A.(fg yellow) "◡◡◡◡◡◡"
    | Sugar -> I.string A.empty "Sugar"
  )

let resource_opt_img tile =
  World.(
    match resource tile with
    | Some r -> resource_img r
    | None -> I.empty
  )

let improvement_str i =
  World.(
    match i with
    | FishingBoats -> "Fishing Boats"
    | Mine -> "Mine"
    | Quarry -> "Quarry"
    | Farm -> "Farm"
    | Camp -> "Camp"
    | Pasture -> "Pasture"
    | Plantation -> "Plantation")

let improvement_opt_str tile =
  World.(
    match improvement tile with
    | Some i -> improvement_str i
    | None -> "")

let tile_yields_img tile =
  World.(
    let y = tile_yields tile in
    I.(hcat [
      uchar A.(fg green) 127823 1 1;
      void 1 1;
      string A.(fg green) (string_of_int y.food);
      void 1 1;
      uchar A.(fg yellow) 11044 1 1;
      void 1 1;
      string A.(fg yellow) (string_of_int y.gold);
      void 1 1;
      uchar A.(fg blue) 128296 1 1;
      void 1 1;
      string A.(fg blue) (string_of_int y.production);
    ])
  )

let tile_unit_str units =
  if List.length units = 0 then ""
  else string_of_int (List.length units) ^ " units"

let city_imgs (col, row) gst =
  match State.city (col, row) gst with
  | Some city ->
    let text s = I.string A.(fg (gray 3)) s in
    let white_text s = I.string A.(fg white ++ st bold) s in
    let pop_attr = A.(fg green ++ st bold) in
    let prod_attr = A.(fg yellow ++ st bold) in
    let top = text "░░░░░░░░░░░░" in
    let mid =
      let background = text "░░░░░░░░░░░░░░" in
      let pop = Entity.population city in
      let pop_frac = float_of_int (Entity.food_stock city)
        /. (float_of_int (Entity.growth_req pop)) in
      let pop_text =
        I.string pop_attr (progress_str pop_frac) in
      let prod_frac = float_of_int (Entity.production_stock city)
        /. (float_of_int (69)) in
      let prod_text =
        I.string prod_attr (progress_str prod_frac) in
      I.(
        hsnap 14 (hcat [
          string pop_attr (string_of_int pop);
          pop_text;
          text "░❰";
          white_text "CITY";
          text "❱░";
          prod_text
        ]) </> background
      )
    in
    let bottom = text "░░░░░░░░░░░░░░░░" in
    (top, mid, bottom)

  | None -> (I.empty, I.empty, I.empty)

let move_unit_tile gst dir =
  let (max_cols, max_rows) = World.map_dimensions gst.map in
  let (col, row) = gst.selected_tile in
  match dir with
  | `TopLeft ->
    if col - 1 > 0 && row > 0 then
      if (col mod 2 = 1) then (col - 1, row)
      else (col - 1, row - 1)
    else (col, row)
  | `TopMiddle -> if row - 1 > 0 && row > 0 then (col, row - 1) else (col, row)
  | `TopRight ->
    if col + 1 < max_cols && row > 0 then
      if (col mod 2 = 1) then (col + 1, row)
      else (col + 1, row - 1)
    else (col, row)
  | `BottomLeft ->
    if col - 1 > 0 && row + 1 < max_rows then
      if (col mod 2 = 1) then
        (col - 1, row - 1)
      else (col - 1, row)
    else (col, row)
  | `BottomMiddle -> if row + 1 < max_rows then (col, row + 1) else (col, row)
  | `BottomRight ->
    if col + 1 < max_cols && row + 1 < max_rows then
      if (col mod 2 = 1) then
        (col + 1, row + 1)
      else (col + 1, row)
    else (col, row)

let tile_img is_selected (col, row) (left_col, top_row) gst (w, h) =
  let color = if is_selected then A.(fg blue) else A.(fg (gray 3)) in
  let text_color = A.(fg white) in
  let color_underline = A.(color ++ st underline) in
  let odd_even_col_offset = if col mod 2 = 1 then (tile_height/2) else 0 in
  let top_underline_offset = if row = top_row || is_selected then 0 else 1 in
  let left = initial_tile_left w + (tile_width*(col - left_col)) in
  let top = initial_tile_top + (tile_height*(row - top_row)) + odd_even_col_offset + top_underline_offset in
  let t = World.get_tile gst.map col row in
  let units = State.units (col, row) gst in
  let (city_top, city_mid, city_bot) = city_imgs (col, row) gst in
  grid [
    if row = top_row then [I.void 5 1; I.string color_underline "            "] 
    else if is_selected then 
      let (above_col, above_row) = move_unit_tile gst `TopMiddle in
      [I.void 5 1; I.hsnap 12 (elevation_img true (World.get_tile gst.map above_col above_row))]
    else [];
    [I.void 4 1; I.uchar color 0x2571 1 1; I.hsnap 12 city_top; I.uchar color 0x2572 1 1];
    [I.void 3 1; I.uchar color 0x2571 1 1; I.hsnap 14 city_mid; I.uchar color 0x2572 1 1];
    [I.void 2 1; I.uchar color 0x2571 1 1; I.hsnap 16 city_bot; I.uchar color 0x2572 1 1];
    [I.void 1 1; I.uchar color 0x2571 1 1; I.hsnap 18 (I.string text_color " "); I.uchar color 0x2572 1 1];
    [I.uchar color 0x2571 1 1; I.hsnap 20 (I.string text_color (tile_unit_str units)); I.uchar color 0x2572 1 1];
    [I.uchar color 0x2572 1 1; I.hsnap 20 I.(I.string text_color (improvement_opt_str t)); I.uchar color 0x2571 1 1];
    [I.void 1 1; I.uchar color 0x2572 1 1; I.hsnap 18 (resource_opt_img t); I.uchar color 0x2571 1 1];
    [I.void 2 1; I.uchar color 0x2572 1 1; I.hsnap 16 (feature_opt_img t); I.uchar color 0x2571 1 1];
    [I.void 3 1; I.uchar color 0x2572 1 1; I.hsnap 14 (terrain_img t); I.uchar color 0x2571 1 1];
    [I.void 4 1; I.uchar color 0x2572 1 1; I.hsnap 12 (elevation_img is_selected t); I.uchar color 0x2571 1 1];
  ] |> I.pad ~l:left ~t:top

let rec game_map_helper img (w, h) gst tiles_w tiles_h (col, row) (left_col, top_row) (map_cols, map_rows) =
  let (next_col, next_row) = if col < min (map_cols - 1) (tiles_w + left_col) then (col + 1, row) else (left_col, row+1) in
  let acc = I.(img </> tile_img false (col, row) (left_col, top_row) gst (w, h)) in
  if next_row <= min (map_rows - 1) (tiles_h + top_row - 1) then
    game_map_helper acc (w, h) gst tiles_w tiles_h (next_col, next_row) (left_col, top_row) (map_cols, map_rows)
  else acc

let game_map (w, h) gst =
  let (tiles_w, tiles_h) = calculate_tiles_w_h (w, h) in
  let (selected_col, selected_row) = gst.selected_tile in
  let (left_col, top_row) = gst.map_display in
  let (map_cols, map_rows) = World.map_dimensions gst.map in
  game_map_helper I.(tile_img true (selected_col, selected_row) (left_col, top_row) gst (w, h)) (w, h) gst tiles_w tiles_h (left_col, top_row) (left_col, top_row) (map_cols, map_rows)

let img t (w, h) gst =
  I.(ui_img (w, h) gst </> game_map (w, h) gst)

let select_tile direction gst =
  let map = State.game_map gst in
  let (max_cols, max_rows) = World.map_dimensions map in
  let (current_col, current_row) = gst.selected_tile in
  match direction with
  | `Up -> (current_col, max 0 (current_row - 1))
  | `Down -> (current_col, min (max_rows - 1) (current_row + 1))
  | `Left -> (max 0 (current_col - 1), current_row)
  | `Right -> (min (max_cols - 1) (current_col + 1), current_row)

let next_pane_state pst tile_changing =
  match pst with
  | Tile -> Tile
  | Unit u -> if tile_changing then Unit 0 else Unit (u+1)
  | City c -> if tile_changing then City 0 else City (c+1)
  | Tech t -> if tile_changing then Tech 0 else Tech (t+1)

let move_unit gst dir =
  match gst.pane_state with
  | Unit u ->
    let (col, row) = gst.selected_tile in
    let units = State.unit_refs (col,row) gst in
    let num_units = List.length units in
    if num_units > 0 then
      let current_unit_num = u mod num_units in
      let current_unit = List.nth units current_unit_num in
      let (new_col, new_row) = move_unit_tile gst dir in
      if (col, row) <> (new_col, new_row) then
        State.make_move gst current_unit (World.get_tile gst.map new_col new_row)
      else gst
    else gst
  | _ -> gst

let rec main t gst =
  let (w, h) = Term.size t in
  Term.image t (img t (w, h) gst);
  match Term.event t with
  | `End | `Key (`Uchar 68, [`Ctrl]) | `Key (`Uchar 67, [`Ctrl])
  | `Key (`Escape, []) -> Quit
  | `Key (`Arrow direction, []) ->
    let (tiles_w, tiles_h) = calculate_tiles_w_h (w, h) in
    let (new_selected_col, new_selected_row) = select_tile direction gst in
    let (current_left_col, current_top_row) = gst.map_display in
    let new_map_display =
      if new_selected_col > (current_left_col + tiles_w) then
        (current_left_col + 2, current_top_row)
      else if new_selected_row > (current_top_row + tiles_h - 1) then
        (current_left_col, current_top_row + 1)
      else if new_selected_col < current_left_col then
        (current_left_col - 2, current_top_row)
      else if new_selected_row < current_top_row then
        (current_left_col, current_top_row - 1)
      else (current_left_col, current_top_row) in
    let new_gst = { gst with
      selected_tile = select_tile direction gst;
      map_display = new_map_display;
      pane_state = next_pane_state gst.pane_state true
    } in
    main t new_gst
  | `Resize (nw, nh) -> main t gst
  | `Key (`Enter, []) -> main t (State.next_turn gst)
  | `Key (`Uchar 117, []) -> main t {gst with pane_state = Unit 0}
  | `Key (`Uchar 99, []) -> main t {gst with pane_state = City 0}
  | `Key (`Uchar 116, []) -> main t {gst with pane_state = Tech 0}
  | `Key (`Uchar 115, []) -> main t {gst with pane_state = Tile}
  | `Key (`Uchar 46, []) -> main t {gst with pane_state = next_pane_state gst.pane_state false}
  | `Key (`Uchar 49, []) -> main t (move_unit gst `TopLeft)
  | `Key (`Uchar 50, []) -> main t (move_unit gst `TopMiddle)
  | `Key (`Uchar 51, []) -> main t (move_unit gst `TopRight)
  | `Key (`Uchar 52, []) -> main t (move_unit gst `BottomLeft)
  | `Key (`Uchar 53, []) -> main t (move_unit gst `BottomMiddle)
  | `Key (`Uchar 54, []) -> main t (move_unit gst `BottomRight)
  | _ -> main t gst

let new_state t gst =
  main t gst