(* The overall map of the game *)
type map

(* Makes up each section of the map *)
type tile

(* The possible terrain features that can exist on a tile *)
type terrain = Grassland | Plains | Desert | Tundra | Ice 
  | Ocean | Coast | Lake

(* The possible natural features of a tile *)
type feature = Forest | Jungle | Oasis | FloodPlains

(* The possible elevations of a tile *)
type elevation = Flatland | Hill | Peak

(* The possible resources present on a tile *)
type resource =
  | Fish | Crab
  | Gold | Silver | Gems | Salt | Iron
  | Marble | Stone
  | Wheat | Corn | Rice
  | Furs | Ivory | Deer
  | Sheep | Cattle | Horses
  | Cotton | Banana | Sugar

(* The possible improvements that can be made to a tile *)
type improvement = FishingBoats | Mine | Quarry | Farm | Camp | Pasture | Plantation

(* The gold, food, and production yields for a tile *)
type tile_yields = {
  gold : int;
  food : int;
  production : int
}

(* [generate_map] creates a new map *)
val generate_map : map

(* [get_tile map col row] gets the tile at the specified coordinates on [map]. *)
val get_tile : map -> int -> int -> tile

(* [map_dimensions map] returns the number of columns and rows in [map]. *)
val map_dimensions : map -> int * int

(* [terrain tile] returns the terrain of [tile]. *)
val terrain : tile -> terrain

(* [feature tile] returns [Some feature] if [tile] has a feature.
 * Returns None otherwise. *)
val feature : tile -> feature option

(* [elevation tile] returns the elevation of [tile]. *)
val elevation : tile -> elevation

(* [resource tile] returns [Some resource] if [tile] contains a resource.
 * Returns None otherwise. *)
val resource : tile -> resource option

(* [improvement tile] returns [Some improvement] if [tile] contains an improvement.
 * Returns None otherwise. *)
val improvement : tile -> improvement option

(* [tile_yields tile] returns the gold, food, and production yields for [tile] *)
val tile_yields : tile -> tile_yields

(* [food_gen tile] returns how much food is generated by [tile] per turn. *)
val food_gen : tile -> int

(* [production_gen tile] returns how much production is generated by [tile] per turn. *)
val production_gen : tile -> int

(* [gold_gen tile] returns how much gold is generated by [tile] per turn. *)
val gold_gen : tile -> int
