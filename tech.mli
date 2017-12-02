(* [tech] represents a researchable technology *)
type tech

(* [tech_cost tech] is the science cost of researching tech *)
val tech_cost : tech -> int

(* [prereqs tech] is the list of prereqs for researching [tech] *)
val prereqs : tech -> tech list
