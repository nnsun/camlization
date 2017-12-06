type gold = int
type time = int
type science = int

let (%!) i j =
  let rem = i mod j in
  if rem < 0 then rem + j
  else rem