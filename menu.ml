open Notty
open Notty_unix
open Notty_helper
open State

let title_img =
  let camlization_width = 67 in
  let v_string = I.string A.(fg yellow) "
8b           d8
`8b         d8'
 `8b       d8'
  `8b     d8'
   `8b   d8'
    `8b d8'
     `888'
      `8'       " in
  let v_void = I.void camlization_width 8 in
  let v_img = I.(v_void <|> v_string <|> v_void) in
  let camlization_lines = [
    I.(I.string A.empty "   #####" <|> (I.void (camlization_width - 8) 1));
    I.(I.string A.empty " #     #   ##   #    # #" <|> (I.void 6 1));
  ] in
  I.vcat camlization_lines

let img t (w, h) mst =
  match mst with
  | Loading -> center (I.string A.(fg lightwhite) "Loading...") w h
  | Copyright ->
    let a1 = A.(fg lightwhite ++ bg red)
    and a2 = A.(fg red) in
    center (I.(string a1 "Copyright" <|> string a2 " stuff!")) w h
  | Main -> center (I.string A.(fg lightwhite) "Main menu") w h
  | Multiplayer options -> center (I.string A.(fg lightwhite) "Multiplayer") w h
  | Options -> center (I.string A.(fg lightwhite) "Options") w h
  | About -> center (I.string A.(fg lightwhite) "About") w h

let rec copyright t (w, h) =
  match Term.event t with
  | `Key (`Enter, []) -> Main
  | `Resize (nw, nh) -> copyright t (nw, nh)
  | _ -> Copyright

let rec main t (w, h) =
  match Term.event t with
  | `End | `Key (`Uchar 68, [`Ctrl]) | `Key (`Uchar 67, [`Ctrl])
  | `Key (`Escape, []) -> Quit
  | `Resize (nw, nh) -> main t (nw, nh)
  | _ -> main t (w, h)

let multiplayer t (w, h) options =
  Game { date = -3000; map = ref World.generate_map }

let new_state t (w, h) mst =
  match mst with
  | Loading -> Unix.sleep 3; Menu(Copyright)
  | Copyright -> Menu(copyright t (w, h))
  | Main -> main t (w, h)
  | Multiplayer options -> multiplayer t (w, h) options
  | Options -> Menu(Options)
  | About -> Menu(About)