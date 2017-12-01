open Notty
open Notty_unix
open State

let img t (w, h) mst =
  match mst with
  | Loading -> I.string A.(fg lightwhite) "Loading..."
  | Copyright ->
    let a1 = A.(fg lightwhite ++ bg red)
    and a2 = A.(fg red) in
    I.(string a1 "Copyright" <|> string a2 " stuff!")
  | Main -> I.string A.(fg lightwhite) "Main menu"
  | Multiplayer options -> I.string A.(fg lightwhite) "Multiplayer"
  | Options -> I.string A.(fg lightwhite) "Options"
  | About -> I.string A.(fg lightwhite) "About"

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