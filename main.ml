open Notty
open Notty_unix
open State

let rec menu t (w, h) mst =
  Term.image t (Menu.img t (w, h) mst);
  main t (w, h) (Menu.new_state t (w, h) mst)
and game t (w, h) gst =
  Term.image t (Gui.img t (w, h) gst);
  main t (w, h) (Gui.new_state t (w, h) gst)
and main t (w, h) st =
  match st with
  | Menu mst -> menu t (w, h) mst
  | Game gst -> game t (w, h) gst
  | Quit -> ()


let start () =
  let t = Term.create () in
  main t (Term.size t) start_state

let _ = start ()