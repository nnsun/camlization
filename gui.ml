open Notty
open Notty_unix
open Notty_helper
open State

let gui_height = 8
let left_padding = 2
let top_padding = 1
let right_padding = 2
let bottom_padding = 2
let gui_bar_padding = 2
let tile_width = 17
let tile_height = 10
let initial_tile_left = left_padding
let initial_tile_top = top_padding

let grid xxs = xxs |> List.map I.hcat |> I.vcat

let ui_img (w, h) gst =
  let player = gst.players.(gst.current_player) in
  let science_text = " ⚕ +" ^ string_of_int (Player.science_rate player) in
  let gold_text = "  ⬤ " ^ string_of_int (Player.gold player) in
  let gold_rate =
    let rate = Player.gold_rate player in
    if rate > 0
    then I.string A.(fg yellow ++ bg black) ("(+" ^ string_of_int rate ^ ")")
    else I.string A.(fg red ++ bg black) (string_of_int rate)
  in
  let metrics = I.hcat [
    I.string A.(fg blue ++ bg black) science_text;
    I.string A.(fg yellow ++ bg black) gold_text;
    gold_rate
  ] in
  I.(metrics </> I.tile w 1 (I.string A.(bg black) " "))

let outline attr t =
  let (w, h) = Term.size t in
  let chr x = I.uchar attr x 1 1
  and hbar = I.uchar attr 0x2500 (w - 2) 1
  and bottom_vbar = I.uchar attr 0x2502 1 (gui_height)
  and top_vbar = I.uchar attr 0x2502 1 (h - 3 - gui_height) in
  let (a, b, c, d) = (chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570) in
  grid [
    [a; hbar; b];
    [top_vbar; I.void (w - 2) 1; top_vbar];
    [chr 0x2502; hbar; chr 0x2502];
    [bottom_vbar; I.void (w - 2) 1; bottom_vbar];
    [d; hbar; c]
  ]

let size_box (cols, rows) =
  let cols_str = string_of_int cols in let rows_str = string_of_int rows in
  let label = (cols_str ^ "x" ^ rows_str) in
  let box = I.string A.(fg lightgreen ++ bg lightblack) label in
  center box cols rows

let calculate_tiles_w_h (w, h) =
  let tiles_w = (float_of_int w -. float_of_int left_padding -.
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
    I.(string A.(fg green) ("🍏 " ^ string_of_int y.food) <|>
       void 1 1 <|>
       string A.(fg yellow) ("⬤ " ^ string_of_int y.gold) <|>
       void 1 1 <|>
       string A.(fg blue) ("🔨 " ^ string_of_int y.production))
  )

let tile_img is_selected (col, row) (left_col, top_row) gst =
  let color = if is_selected then A.(fg blue) else A.(fg white) in
  let text_color = A.(fg white) in
  let color_underline = A.(color ++ st underline) in
  let odd_even_col_offset = if col mod 2 = 1 then (tile_height/2) else 0 in
  let top_underline_offset = if row = top_row || is_selected then 0 else 1 in
  let left = initial_tile_left + (tile_width*(col - left_col)) in
  let top = initial_tile_top + (tile_height*(row - top_row)) + odd_even_col_offset + top_underline_offset in
  let tile = World.get_tile !(gst.map) col row in
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

let rec game_map_helper img gst tiles_w tiles_h (col, row) (left_col, top_row) (map_cols, map_rows) =
  let (next_col, next_row) = if col < min (map_cols - 1) (tiles_w + left_col) then (col + 1, row) else (left_col, row+1) in
  let acc = I.(img </> tile_img false (col, row) (left_col, top_row) gst) in
  if next_row <= min (map_rows - 1) (tiles_h + top_row - 1) then
    game_map_helper acc gst tiles_w tiles_h (next_col, next_row) (left_col, top_row) (map_cols, map_rows)
  else acc

let game_map (w, h) gst =
  let (tiles_w, tiles_h) = calculate_tiles_w_h (w, h) in
  let (selected_col, selected_row) = gst.selected_tile in
  let (left_col, top_row) = gst.map_display in
  let (map_cols, map_rows) = World.map_dimensions !(gst.map) in
  game_map_helper I.(tile_img true (selected_col, selected_row) (left_col, top_row) gst) gst tiles_w tiles_h (left_col, top_row) (left_col, top_row) (map_cols, map_rows)

let img t (w, h) gst = I.(ui_img (w, h) gst </> game_map (w, h) gst)

let select_tile direction gst =
  let map = State.game_map gst in
  let (max_cols, max_rows) = World.map_dimensions map in
  let (current_col, current_row) = gst.selected_tile in
  match direction with
  | `Up -> (current_col, max 0 (current_row - 1))
  | `Down -> (current_col, min (max_rows - 1) (current_row + 1))
  | `Left -> (max 0 (current_col - 1), current_row)
  | `Right -> (min (max_cols - 1) (current_col + 1), current_row)

let rec main t (w, h) gst =
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
    let new_gst = {gst with selected_tile = select_tile direction gst;
                            map_display = new_map_display} in
    Term.image t (img t (w, h) new_gst); main t (w, h) new_gst
  | `Resize (nw, nh) -> Term.image t (img t (nw, nh) gst); main t (nw, nh) gst
  | _ -> main t (w, h) gst

let new_state t (w, h) gst =
  main t (w, h) gst