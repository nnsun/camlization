type menu_state = {
  player_count_menu_open: bool;
  player_count: int
}

type game_state = {
  date: int;
  map: World.map
}

type state =
  | Menu of menu_state
  | Game of game_state

type gold = int

let date gst =
  let date = gst.date in
  let suffix = if date < 0 then " BCE" else " CE" in
  string_of_int date ^ suffix

let game_map gst = gst.map