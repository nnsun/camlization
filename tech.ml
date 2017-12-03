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

type tech_info = {
  cost : int;
  prereqs : tech list
}

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