(* Type aliases for gold, time, and science *)
type gold = int
type time = int
type science = int

(* Game constants *)
let initial_year = -3000
let years_per_turn = 100
let max_players = 8

(* [i %! j] is the remaineder of i / j, with special rules:
 * - If [j = 0], then returns 0.
 * - If [(i mod j) < 0], then returns [(i mod j) + j]
 * - Returns [i mod j] otherwise *)
let (%!) i j =
  if j = 0 then 0
  else
    let rem = i mod j in
    if rem < 0 then rem + j
    else rem