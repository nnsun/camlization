open Notty

type gold = int
type time = int
type science = int

let initial_year = -3000
let years_per_turn = 100
let max_players = 8

let (%!) i j =
  if j = 0 then 0
  else
    let rem = i mod j in
    if rem < 0 then rem + j
    else rem