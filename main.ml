open Notty
open Notty_unix
open State

(* Functions that keep track of the REPL, switching between menu and game states *)
let rec menu t mst =
  main t (Menu.new_state t mst)
and game t gst =
  main t (Gui.new_state t gst)
and main t (st: state) =
  match st with
  | Menu mst -> menu t mst
  | Game gst -> game t gst
  | Quit -> ()

(* [start] begins the REPL. *)
let start () =
  let t = Term.create () in
  main t start_state

let _ = start ()