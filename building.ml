open Tech

type building = {
  cost : int;
  building_prereq : building option;
  tech_prereq : tech option;
  science_mult : float;
  science : int;
  gold_mult : float;
  gold : int;
  food_mult : float;
  food : int;
  production_mult : float;
  production : int;
  culture_mult : float;
  culture : int;
  faith : int;
  defense : int;
  experience : int;
}

let cost b = b.cost

let building_prereq b = b.building_prereq

let tech_prereq b = b.tech_prereq

let science_mult b = b.science_mult

let science b = b.science

let gold_mult b = b.gold_mult

let gold b = b.gold

let food_mult b = b.food_mult

let food b = b.food

let production_mult b = b.production_mult

let production b = b.production

let culture_mult b = b.culture_mult

let culture b = b.culture

let faith b = b.faith

let defense b = b.defense

let experience b = b.experience