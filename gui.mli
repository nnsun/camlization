open Notty
open State

val img : Notty_unix.Term.t -> int * int -> game_state -> image

val new_state : Notty_unix.Term.t -> int * int -> game_state -> state