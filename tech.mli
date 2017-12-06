(* [tech] represents a researchable technology *)
type tech =
  | Agriculture
    | Fishing
      | Sailing
        | Optics
    | AnimalHusbandry
      | Trapping
        | HorsebackRiding
    | Mining
      | Masonry
        | Calendar
      | BronzeWorking
        | IronWorking
    | Archery
      | TheWheel
        | Mathematics

(* [tech_cost tech] is the science cost of researching tech *)
val tech_cost : tech -> int

(* [prereqs tech] is the list of prereqs for researching [tech] *)
val prereqs : tech -> tech list

(* [tech_list] is a list of all techs *)
val tech_list : tech list

(* [improvements_for_tech tech] is a list of possible improvements
 * which [tech] enables a player to build *)
 val improvements_for_tech : tech -> World.improvement option