(* [entity] represents the various units/entities in the game *)
type entity

(* [entity_state] represents the state for an entity, such as working or asleep *)
type entity_state

(* [entity_action] represents an action that can be taken by an entity *)
type entity_action

(* [owner entity] is the player who owns [entity] *)
val owner : entity -> Player.player

(* [health entity] is the health of [entity] (>= 0) *)
val health : entity -> int

(* [movement_points entity] is the remaining movement points of [entity] (>= 0) *)
val movement_points : entity -> int
