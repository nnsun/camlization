type gold = int
type time = int
type science = int

let (%!) i j =
  if j = 0 then 0
  else
    let rem = i mod j in
    if rem < 0 then rem + j
    else rem