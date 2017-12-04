open Notty
open Notty_unix
open Notty_helper
open State

(* Constants and Helpers *)

let is_mac_os = true

let left_padding = 2
let top_padding = 1
let right_padding = 2
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
  let science_text = string_of_int (Player.science_rate player) in
  let gold_text = string_of_int (Player.gold player) in
  let gold_rate =
    let rate = Player.gold_rate player in
    if rate >= 0
      then I.string A.(fg yellow ++ bg black) ("(+" ^ string_of_int rate ^ ")")
    else I.string A.(fg red ++ bg black) ("(" ^ string_of_int rate ^ ")")
  in
  let status = I.hsnap ?align:(Some `Right) w (I.hcat [
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
  I.(void 1 (h - 4) <-> hsnap w bar)

let left_pane (w, h) gst =
  let pane_width = pane_width w in
  let text = A.(fg white ++ bg black) in
  I.zcat [
    I.vcat [
      I.hsnap pane_width (I.(string A.(text ++ st underline)) "UNITS");
      I.void 1 1;
      I.hsnap pane_width (I.string text "MOVEMENT:");
      I.hsnap pane_width (I.hcat [I.uchar text 8598 1 1; (I.string text " 1 ");
                                  I.uchar text 8593 1 1; (I.string text " 2 ");
                                  I.uchar text 8599 1 1; (I.string text " 3 ")]);
      I.hsnap pane_width (I.hcat [I.uchar text 8601 1 1; (I.string text " 6 ");
                                  I.uchar text 8595 1 1; (I.string text " 5 ");
                                  I.uchar text 8600 1 1; (I.string text " 4 ");
                                  ]);
      I.void 1 1;
      I.hsnap pane_width (I.string text "HEALTH: ");
      I.void 1 1;
      I.hsnap pane_width (I.string text "SELECT UNIT WITH U")];
    I.(char A.(fg white ++ bg black) ' ' pane_width (h - top_padding - bottom_padding))
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

let terrain_str tile =
  World.(
    match terrain tile with
    | Grassland -> "Grassland"
    | Plains -> "Plains"
    | Desert -> "Desert"
    | Tundra -> "Tundra"
    | Ice -> "Ice"
    | Ocean -> "Ocean"
    | Coast -> "Coast"
    | Lake -> "Lake")

let feature_str f =
  World.(
    match f with
    | Forest -> "Forest"
    | Jungle -> "Jungle"
    | Oasis -> "Oasis"
    | FloodPlains -> "Flood Plains")

let feature_opt_str tile =
  World.(
    match feature tile with
    | Some f -> feature_str f
    | None -> "")

let elevation_str tile =
  World.(
    match elevation tile with
    | Flatland -> "Flatland"
    | Hill -> "Hill"
    | Peak -> "Peak")

let resource_str r = (* TODO: Make sure player has researched it *)
  World.(
    match r with
    | Fish -> "Fish"
    | Crab -> "Crab"
    | Gold -> "Gold"
    | Silver -> "Silver"
    | Gems -> "Gem"
    | Salt -> "Salt"
    | Iron -> "Iron"
    | Marble -> "Marble"
    | Stone -> "Stone"
    | Wheat -> "Wheat"
    | Corn -> "Corn"
    | Rice -> "Rice"
    | Furs -> "Furs"
    | Ivory -> "Ivory"
    | Deer -> "Deer"
    | Sheep -> "Sheep"
    | Cattle -> "Cattle"
    | Horses -> "Horses"
    | Cotton -> "Cotton"
    | Banana -> "Banana"
    | Sugar -> "Sugar")

let resource_opt_str tile =
  World.(
    match resource tile with
    | Some r -> resource_str r
    | None -> "")

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

let tile_img is_selected (col, row) (left_col, top_row) gst (w, h) =
  let color = if is_selected then A.(fg blue) else A.(fg white) in
  let text_color = A.(fg white) in
  let color_underline = A.(color ++ st underline) in
  let odd_even_col_offset = if col mod 2 = 1 then (tile_height/2) else 0 in
  let top_underline_offset = if row = top_row || is_selected then 0 else 1 in
  let left = initial_tile_left w + (tile_width*(col - left_col)) in
  let top = initial_tile_top + (tile_height*(row - top_row)) + odd_even_col_offset + top_underline_offset in
  let tile = World.get_tile gst.map col row in
  grid [
    if row = top_row || is_selected then [I.void 5 1; I.string color_underline "            "] else [];
    [I.void 4 1; I.uchar color 0x2571 1 1; I.string color "            "; I.uchar color 0x2572 1 1];
    [I.void 3 1; I.uchar color 0x2571 1 1; I.string color "              "; I.uchar color 0x2572 1 1];
    [I.void 2 1; I.uchar color 0x2571 1 1; I.hsnap 16 (I.string text_color (terrain_str tile)); I.uchar color 0x2572 1 1];
    [I.void 1 1; I.uchar color 0x2571 1 1; I.hsnap 18 (I.string text_color (feature_opt_str tile)); I.uchar color 0x2572 1 1];
    [I.uchar color 0x2571 1 1; I.hsnap 20 (I.string text_color (elevation_str tile)); I.uchar color 0x2572 1 1];
    [I.uchar color 0x2572 1 1; I.hsnap 20 (I.string text_color (resource_opt_str tile)); I.uchar color 0x2571 1 1];
    [I.void 1 1; I.uchar color 0x2572 1 1; I.hsnap 18 (I.string text_color (improvement_opt_str tile)); I.uchar color 0x2571 1 1];
    [I.void 2 1; I.uchar color 0x2572 1 1; I.hsnap 16 (tile_yields_img tile); I.uchar color 0x2571 1 1];
    [I.void 3 1; I.uchar color 0x2572 1 1; I.string color "              "; I.uchar color 0x2571 1 1];
    [I.void 4 1; I.uchar color 0x2572 1 1; I.string color_underline "            "; I.uchar color 0x2571 1 1];
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
      map_display = new_map_display
    } in
    main t new_gst
  | `Resize (nw, nh) -> main t gst
  | `Key (`Enter, []) -> main t gst
  | _ -> main t gst

let new_state t gst =
  main t gst