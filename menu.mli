open Notty
open State

val img : Notty_unix.Term.t -> int * int -> menu_state -> image

val new_state : Notty_unix.Term.t -> int * int -> menu_state -> state