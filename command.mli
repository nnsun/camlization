(* [command] represents a command input by a player. *)
type command

(* [parse str] is the command that represents the player input [str]. *)
val parse : string -> command
