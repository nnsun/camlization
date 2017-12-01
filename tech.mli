(* [tech] represents a researchable technology *)
type tech

module TechSet : Set.S

(* [cost tech] is the science cost of researching tech *)
val cost : tech -> int
