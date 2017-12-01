open Notty
open Notty_unix
open State

let gui_height = 8

let grid xxs = xxs |> List.map I.hcat |> I.vcat

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

let size_box cols rows =
  let cols_str = string_of_int cols in let rows_str = string_of_int rows in
  let label = (cols_str ^ "x" ^ rows_str) in
  let box = I.string A.(fg lightgreen ++ bg lightblack) label in
  let top_margin = (rows - I.height box) / 2 in
  let left_margin = (cols - I.width box) / 2 in
  I.pad ~t:top_margin ~l:left_margin box

let img t (w, h) gst = I.((outline A.(fg lightred ) t) </> (size_box w h))

let new_state t (w, h) gst =
  Game (gst)