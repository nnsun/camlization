open Primitives

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

(* Stores the cost and prereq info for techs *)
type tech_info = {
  cost : int;
  prereqs : tech list
}

(* Map of techs to underlying info *)
let tech_info_map = [
  Agriculture, { cost = 20; prereqs = [] };
  Fishing, { cost = 35; prereqs = [Agriculture] };
  AnimalHusbandry, { cost = 35; prereqs = [Agriculture] };
  Mining, { cost = 35; prereqs = [Agriculture] };
  Archery, { cost = 35; prereqs = [Agriculture] };
  Sailing, { cost = 55; prereqs = [Fishing] };
  Trapping, { cost = 55; prereqs = [AnimalHusbandry] };
  Masonry, { cost = 55; prereqs = [Mining] };
  BronzeWorking, { cost = 55; prereqs = [Mining] };
  TheWheel, { cost = 55; prereqs = [Archery] };
  Optics, { cost = 85; prereqs = [Sailing] };
  HorsebackRiding, { cost = 85; prereqs = [Trapping] };
  Calendar, { cost = 85; prereqs = [Masonry] };
  IronWorking, { cost = 85; prereqs = [BronzeWorking] };
  Mathematics, { cost = 85; prereqs = [TheWheel] };
]

let tech_cost tech = (List.assoc tech tech_info_map).cost

let prereqs tech = (List.assoc tech tech_info_map).prereqs

let tech_list =
  let rec prepend acc lst =
    match lst with
    | [] -> acc
    | a::b -> prepend ((fst a)::acc) b in
  prepend [] tech_info_map

let improvements_for_tech = World.(function
  | Mining -> Some Mine
  | Fishing -> Some FishingBoats
  | Masonry -> Some Quarry
  | Agriculture -> Some Farm
  | Trapping -> Some Camp
  | AnimalHusbandry -> Some Pasture
  | Calendar -> Some Plantation
  | IronWorking | Sailing | Optics | HorsebackRiding | Mathematics | Archery 
    | BronzeWorking | TheWheel -> None)

let resources_for_tech = World.(function
  | Mining -> [Gold; Silver; Gems; Salt]
  | Fishing -> [Fish; Crab]
  | Masonry -> [Marble; Stone]
  | Agriculture -> [Wheat; Corn; Rice]
  | Trapping -> [Furs; Ivory; Deer]
  | AnimalHusbandry -> [Horses; Sheep; Cattle]
  | Calendar -> [Cotton; Banana; Sugar]
  | IronWorking -> [Iron]
  | Sailing | Optics | HorsebackRiding | Mathematics | Archery 
    | BronzeWorking | TheWheel -> [])