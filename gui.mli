open Notty
open State

(* [new_state t st] returns the next state based on user interaction in [st] *)
val new_state : Notty_unix.Term.t -> game_state -> state