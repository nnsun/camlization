open Notty
open Notty_unix
open Notty_helper
open State

let gui_height = 8
let top_bar_height = 2
let left_padding = 2
let top_padding = 2
let right_padding = 2
let bottom_padding = 2
let gui_bar_padding = 2
let tile_width = 17
let tile_height = 10
let initial_tile_left = left_padding
let initial_tile_top = top_padding + top_bar_height

let grid xxs = xxs |> List.map I.hcat |> I.vcat

let ui_img gst =
  let player = gst.players.(gst.current_player) in
  let science_text = "+" ^ string_of_int (Player.science_rate player) ^ " " in
  let gold_text = " ⬤ " ^ string_of_int (Player.gold player) in
  let gold_rate =
    let rate = Player.gold_rate player in
    if rate > 0 then I.string A.(fg yellow) ("(+" ^ string_of_int rate ^ ")")
    else I.string A.(fg red) (string_of_int rate)
  in
  I.hcat [
    I.string A.(fg lightblue ++ bg black) " ⚕ ";
    I.string A.(fg blue ++ bg black) science_text;
    I.string A.(fg yellow) (gold_text);
    gold_rate
  ]

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
  let tiles_w = (w - left_padding - right_padding - tile_width) / tile_width in
  let tiles_h = (h - top_padding - bottom_padding - top_bar_height - gui_bar_padding - tile_height) / tile_height in
  (tiles_w, tiles_h)

let tile_img is_selected (col, row) (left_col, top_row) =
  let color = if is_selected then A.(fg blue) else A.(fg white) in
  let color_underline = A.(color ++ st underline) in
  let odd_even_col_offset = if col mod 2 = 1 then (tile_height/2) else 0 in
  let top_underline_offset = if row = top_row || is_selected then 0 else 1 in
  let left = initial_tile_left + (tile_width*(col - left_col)) in
  let top = initial_tile_top + (tile_height*(row - top_row)) + odd_even_col_offset + top_underline_offset in
  let col_str = if col < 10 then "   " ^ string_of_int col else "  " ^ string_of_int col in
  let row_str = if row < 10 then "   " ^ string_of_int row else "  " ^ string_of_int row in
  grid [
    if row = top_row || is_selected then [I.void 5 1; I.string color_underline "            "] else [];
    [I.void 4 1; I.uchar color 0x2571 1 1; I.string color "            "; I.uchar color 0x2572 1 1];
    [I.void 3 1; I.uchar color 0x2571 1 1; I.string color "              "; I.uchar color 0x2572 1 1];
    [I.void 2 1; I.uchar color 0x2571 1 1; I.string color "                "; I.uchar color 0x2572 1 1];
    [I.void 1 1; I.uchar color 0x2571 1 1; I.string color "                  "; I.uchar color 0x2572 1 1];
    [I.uchar color 0x2571 1 1; I.string color col_str; I.string color "                "; I.uchar color 0x2572 1 1];
    [I.uchar color 0x2572 1 1; I.string color row_str; I.string color "                "; I.uchar color 0x2571 1 1];
    [I.void 1 1; I.uchar color 0x2572 1 1; I.string color "                  "; I.uchar color 0x2571 1 1];
    [I.void 2 1; I.uchar color 0x2572 1 1; I.string color "                "; I.uchar color 0x2571 1 1];
    [I.void 3 1; I.uchar color 0x2572 1 1; I.string color "              "; I.uchar color 0x2571 1 1];
    [I.void 4 1; I.uchar color 0x2572 1 1; I.string color_underline "            "; I.uchar color 0x2571 1 1];
  ] |> I.pad ~l:left ~t:top

let rec game_map_helper img tiles_w tiles_h (col, row) (left_col, top_row) =
  let (next_col, next_row) = if col < (tiles_w + left_col) then (col + 1, row) else (left_col, row+1) in
  let acc = I.(img </> tile_img false (col, row) (left_col, top_row)) in
  if next_row < (tiles_h + top_row) then
    game_map_helper acc tiles_w tiles_h (next_col, next_row) (left_col, top_row)
  else acc

let game_map (w, h) gst =
  let (tiles_w, tiles_h) = calculate_tiles_w_h (w, h) in
  let (selected_col, selected_row) = gst.selected_tile in
  let (left_col, top_row) = gst.map_display in
  game_map_helper I.(tile_img true (selected_col, selected_row) (left_col, top_row)) tiles_w tiles_h (left_col, top_row) (left_col, top_row)

let img t (w, h) gst = I.((outline A.(fg lightred ) t) </> game_map (w, h) gst)

let select_tile direction gst =
  let map = State.game_map gst in
  let (max_cols, max_rows) = World.map_dimensions map in
  let (current_col, current_row) = gst.selected_tile in
  match direction with
  | `Up -> (current_col, max 0 (current_row - 1))
  | `Down -> (current_col, min max_rows (current_row + 1))
  | `Left -> (max 0 (current_col - 1), current_row)
  | `Right -> (min max_cols (current_col + 1), current_row)

let rec main t (w, h) gst =
  match Term.event t with
  | `End | `Key (`Uchar 68, [`Ctrl]) | `Key (`Uchar 67, [`Ctrl])
  | `Key (`Escape, []) -> Quit
  | `Key (`Arrow direction, []) ->
    let (tiles_w, tiles_h) = calculate_tiles_w_h (w, h) in
    let (new_selected_col, new_selected_row) = select_tile direction gst in
    let (current_left_col, current_top_row) = gst.map_display in
    let new_map_display = 
      if new_selected_col > (current_left_col + tiles_w - 1) then
        (current_left_col + 1, current_top_row)
      else if new_selected_row > (current_top_row + tiles_h - 1) then
        (current_left_col, current_top_row + 1)
      else (min current_left_col new_selected_col,
            min current_top_row new_selected_row) in
    let new_gst = {gst with selected_tile = select_tile direction gst;
                            map_display = new_map_display} in
    Term.image t (img t (w, h) new_gst); main t (w, h) new_gst
  | `Resize (nw, nh) -> Term.image t (img t (nw, nh) gst); main t (nw, nh) gst
  | _ -> main t (w, h) gst

let new_state t (w, h) gst =
  main t (w, h) gst