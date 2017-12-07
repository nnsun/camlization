open Notty
open Notty_unix
open Notty_helper
open State
open Primitives

(* Constants and Helpers *)

let is_mac_os = true

let left_padding = 1
let top_padding = 1
let right_padding = 0
let bottom_padding = 4
let gui_bar_padding = 2
let tile_width = 17
let tile_height = 10
let left_pane_frac = 0.25

(* [player_color] p is the player color for the current_player *)
let player_color gst p =
  let rec index i = function
  | [] -> failwith "Player not found"
  | h::t -> if h = p then i else index (i+1) t
  in
  player_colors.(index 0 (Array.to_list gst.players))

(* Calculations for the positioning/size of the left pane and tiles *)
let pane_width w =
  (float_of_int w) *. left_pane_frac |> int_of_float

let initial_tile_left w =
  left_padding + (pane_width w)
let initial_tile_top = top_padding

(* [grid xxs] concatenates images in [xxs] horizontally and vertically to form a new image *)
let grid xxs = xxs |> List.map I.hcat |> I.vcat

(* [status_bar (W, h) gst] is the status bar image *)
let status_bar (w, h) gst =
  let player = gst.players.(gst.current_player) in
  let science_text = "+" ^ string_of_int (Player.science_rate player) in
  let gold_text = string_of_int (Player.gold player) in
  let gold_rate =
    let rate = Player.gold_rate player in
    if rate >= 0
      then I.string A.(fg yellow ++ bg black) ("(+" ^ string_of_int rate ^ ")")
    else I.string A.(fg red ++ bg black) ("(" ^ string_of_int rate ^ ")")
  in
  let status = I.hsnap ~align:`Right w (I.hcat [
    I.string A.(empty ++ bg black) ("Turns: " ^ string_of_int (State.turns gst));
    I.string A.(empty ++ bg black) (
      "  "
      ^ date gst
    );
    I.string A.(empty ++ bg black) "  QUIT: ESC "
  ]) in
  let metrics = I.hcat [
    I.void 1 1;
    I.uchar A.(fg blue ++ bg black) 9877 1 1;
    I.string A.(fg blue ++ bg black) science_text;
    I.void 1 1;
    I.uchar A.(fg yellow ++ bg black) 11044 1 1;
    I.void 1 1;
    I.string A.(fg yellow ++ bg black) gold_text;
    gold_rate
  ] in
  let background = I.tile w 1 (I.string A.(bg black) " ") in
  I.(metrics </> status </> background)

(* [player_bar (W, h) gst] is the player bar image *)
let player_bar (w, h) gst =
  let pimg i p =
    let player_string = " PLAYER " ^ string_of_int (i+1) ^ " " in
    let len = String.length player_string in
    if gst.current_player = i
    then
      let color = player_colors.(gst.current_player) in
      I.(vcat [
        hsnap len (uchar A.(fg white) 9660 1 1);
        string color (String.make len ' ');
        string color player_string;
        string color (String.make len ' ');
      ])
    else
      I.(vcat [
        void 1 1;
        string A.empty (String.make len ' ');
        string A.empty player_string;
        string A.empty (String.make len ' ');
      ])
  in
  let bar = Array.mapi pimg gst.players |> Array.to_list |> I.hcat in
  I.(pad ~l:(pane_width w) ~t:(h - 4) (hsnap (w - pane_width w) bar))

(* [tile_yields_img tile] is the image displayed on the left pane's tile screen,
 * displaying food, gold, and production yields for the [tile]. *)
let tile_yields_img tile =
  World.(
    let y = tile_yields tile in
    I.(hcat [
      uchar A.(fg green) 127823 1 1;
      void 1 1;
      string A.(fg green) (string_of_int y.food);
      void 1 1;
      uchar A.(fg yellow) 11044 1 1;
      void 1 1;
      string A.(fg yellow) (string_of_int y.gold);
      void 1 1;
      uchar A.(fg blue) 128296 1 1;
      void 1 1;
      string A.(fg blue) (string_of_int y.production);
    ]))

(* [progress_str f] is the string that indicates the progress on a city tile *)
let progress_str f =
  if f < 0.15 then "▁"
  else if f < 0.30 then "▂"
  else if f < 0.45 then "▃"
  else if f < 0.60 then "▅"
  else if f < 0.75 then "▆"
  else if f < 0.90 then "▇"
  else "█"

(* [tech_str t] returns the constructor's name for tech [t] *)
let tech_str t =
  Tech.(
    match t with
    | Agriculture -> "Agriculture"
    | Fishing -> "Fishing"
    | Sailing -> "Sailing"
    | Optics -> "Optics"
    | AnimalHusbandry -> "Animal Husbandry"
    | Trapping -> "Trapping"
    | HorsebackRiding -> "Horseback Riding"
    | Mining -> "Mining"
    | Masonry -> "Masonry"
    | Calendar -> "Calendar"
    | BronzeWorking -> "Bronze Working"
    | IronWorking -> "Iron Working"
    | Archery -> "Archery"
    | TheWheel -> "The Wheel"
    | Mathematics -> "Mathematics"
  )
(* [unit_type_str utype] returns the constructor's name for unit type [utype] *)
let unit_type_str utype =
  Entity.(
    match utype with
    | Worker -> "Worker"
    | Scout -> "Scout"
    | Warrior -> "Warrior"
    | WorkBoat -> "Work Boat"
    | Archer -> "Archer"
    | Trireme -> "Trireme"
    | Spearman -> "Spearman"
    | Chariot -> "Chariot"
    | Horseman -> "Horseman"
    | Swordsman -> "Swordsman"
    | Catapult -> "Catapult")

(* [unit_str u] returns the string representation of the unit [u] *)
let unit_str u = unit_type_str (Entity.unit_type u)

(* [improvement_str i] returns the constructor's name for improvement [i] *)
let improvement_str i =
  World.(
    match i with
    | FishingBoats -> "Fishing Boats"
    | Mine -> "Mine"
    | Quarry -> "Quarry"
    | Farm -> "Farm"
    | Camp -> "Camp"
    | Pasture -> "Pasture"
    | Plantation -> "Plantation")

(* [improvement_opt_str tile] returns the string representation of [tile]'s
 * improvement, if present. "" otherwise. *)
let improvement_opt_str tile =
  World.(
    match improvement tile with
    | Some i -> improvement_str i
    | None -> "")

(* [unit_list_img gst ul selected_unit] returns the image with a list of units for the selected tile *)
let unit_list_img gst ul selected_unit =
  let rec unit_list_img_helper ul selected_unit current_unit =
    let health = A.(fg red ++ bg black) in
    match ul with
    | [] -> I.void 1 1
    | (p, u) :: us ->
      let unit = Entity.Unit u in
      let text blink =
        if selected_unit = current_unit
        then
          if blink then A.(fg white ++ bg black ++ st bold ++ st blink)
          else A.(fg white ++ bg black ++ st bold)
        else A.(fg white ++ bg black) in
      let arrow =
        if selected_unit = current_unit
        then I.(I.uchar (text true) 9654 1 1 <|> I.void 1 1)
        else I.void 2 1 in
      I.vcat [
        I.hcat [
          arrow;
          I.string (text false) (unit_str u);
          I.void 1 1;
          I.uchar health 9829 1 1;
          I.void 1 1;
          I.string health (string_of_int (Entity.health unit));
          I.void 1 1;
          I.uchar (text false) 8599 1 1;
          I.void 1 1;
          I.string (text false) (string_of_int (Entity.moves_left u));
          I.void 1 1;
          (
            let str = if gst.players.(gst.current_player) = p
              then ("YOU")
              else ("P" ^ (string_of_int (State.player_number gst p)))
            in
            I.string (player_color gst p) str
          )
        ];
        unit_list_img_helper us selected_unit (current_unit+1)
      ] in
  unit_list_img_helper ul selected_unit 0

(* [tab_img pst selected] returns the image with the left pane navigation tabs *)
let tab_img pst selected =
  let title_string =
    match pst with
    | Unit _ -> " UNITS [1] "
    | City _ -> " CITY [2] "
    | Tech _ -> " TECH [3] "
  in
  let len = String.length title_string in
  I.(void 1 1 <-> (
    if selected
    then I.(vcat [
      string A.(bg white) (String.make len ' ');
      string A.(fg black ++ bg white) title_string;
      string A.(bg white) (String.make len ' ');
    ])
    else I.(vcat [
      string A.(bg black) (String.make len ' ');
      string A.(bg black) title_string;
      string A.(bg black) (String.make len ' ');
    ])
  ))

(* [possible_improvements gst tile] returns a list of possible improvements for [tile] *)
let possible_improvements gst tile =
  let tile_possible_improvements = World.tile_possible_improvements tile in
  let current_player = gst.players.(gst.current_player) in
  Player.available_improvements current_player
    |> List.filter (fun i -> if i = World.Farm &&
      World.feature tile = Some World.Forest ||
      World.feature tile = Some World.Jungle then
      List.mem Tech.IronWorking (Player.techs current_player)
    else List.mem i tile_possible_improvements)

(* [visible_resource gst tile] returns the resource visible to the current player,
 * if possible. None otherwise. *)
let visible_resource gst tile =
  let current_player = gst.players.(gst.current_player) in
  let all_visible_resources =
    List.map Tech.resources_for_tech (Player.techs current_player)
    |> List.flatten in
  match World.resource tile with
  | None -> None
  | Some r -> if List.mem r all_visible_resources then Some r else None

(* [possible_improvements_img pi selected_pi] returns the image with a list of possible improvements for [tile] *)
let possible_improvements_img pi selected_pi =
  let rec possible_improvements_img_helper pi selected_pi current_pi =
    match pi with
    | [] -> I.void 1 1
    | i :: is ->
      let text blink =
        if selected_pi = current_pi
        then
          if blink then A.(fg white ++ bg black ++ st bold)
          else A.(fg white ++ bg black)
        else A.(fg white ++ bg black) in
      let arrow =
        if selected_pi = current_pi
        then I.(I.uchar (text true) 9654 1 1 <|> I.void 1 1)
        else I.void 2 1 in
      I.vcat [
        I.hcat [
          arrow;
          I.string (text false) (improvement_str i)
        ];
        possible_improvements_img_helper is selected_pi (current_pi+1)
      ] in
  possible_improvements_img_helper pi selected_pi 0

(* [left_pane (w, h) gst] is the image for the left pane, based on the current [gst] *)
let left_pane (w, h) gst =
  let player = gst.players.(gst.current_player) in
  let pane_width = pane_width w in
  let snap img = I.hsnap pane_width img in
  let text = A.(fg white ++ bg black) in
  let underlined = A.(fg white ++ bg black ++ st underline) in
  let (col, row) = gst.selected_tile in
  let tile = World.get_tile gst.map col row in
  let pane_content = match gst.pane_state with
  | Unit (u,i) ->
    let units = State.units (col, row) gst in
    let num_units = List.length units in
    let possible_improvements = possible_improvements gst tile in
    let num_possible_improvements = List.length possible_improvements in
    let u, i = u %! num_units, i %! num_possible_improvements in
    let show_improvements = num_units > 0 && num_possible_improvements > 0 &&
      Entity.unit_type (snd (List.nth units u)) = Entity.Worker in
    let tabs = [Unit (u, i); City 0; Tech 0] in
    I.vcat [
      snap (I.hcat (List.map (fun t -> tab_img t (t = ( Unit (u,i) ))) tabs));
      I.void 1 1;
      if num_units > 0 then I.vcat [
          I.hsnap pane_width (
            I.hsnap ~align: `Left (pane_width - 8) (unit_list_img gst units u)
          );
          I.void 1 1;
          snap (I.string text "MOVEMENT:");
          snap (I.string text "___");
          snap (I.string text "/   \\");
          snap (I.string text ",--(     )--.");
          snap (I.(hcat [
            string text "/    \\"; string underlined "[W]"; string text "/    \\"
          ]));
          snap (I.string text "\\ [Q]/   \\[E] /");
          snap (I.string text ")--(     )--(");
          snap (I.string text "/ [A]\\___/[D] \\");
          snap (I.string text "\\    /[S]\\    /");
          snap (I.string text "`--(     )--'");
          snap (I.string text "\\___/");
          I.void 1 1;
          I.vcat [
            snap (I.string text "Found city with [F]");
            I.void 1 1;
            snap (I.string text "Select unit with [J] and [K]");
            I.void 1 2;
            if show_improvements then I.vcat [
              snap (I.string text "IMPROVEMENTS:");
              I.void 1 1;
              snap (
                I.hsnap ~align: `Left (pane_width - 8)
                  (possible_improvements_img possible_improvements i);
              );
              I.void 1 1;
              snap (I.string text "Select improvement with [ and ]");
              I.void 1 1;
              snap (I.string text "Build improvement with [I]")]
            else I.void 1 1;
          ]
      ] else snap (I.string text "NO UNITS IN THIS TILE")
    ]
  | City c ->
    let tabs = [Unit (0, 0); City c; Tech 0] in
    let empty_city =
      I.vcat [
        snap (I.hcat (List.map (fun t -> tab_img t (t = (City c))) tabs));
        I.void 1 1;
        snap (I.string text "NO CITY AT THIS TILE")
      ]
    in
    begin
      match State.city_ref (col, row) gst with
      | Some (p, entity_ref) ->
        if p != player then empty_city else
        let city = Entity.get_city_entity !entity_ref in
        let current_unit = Entity.unit_production city in
        let units = State.available_units gst in
        let turns_left u =
          if Entity.production_per_turn city = 0 then 99
          else
            max 1 ((Entity.unit_cost u - Entity.production_stock city)
            / (Entity.production_per_turn city))
          in
        let unit_img i u show show_selected =
          if i = c %! List.length units && show && show_selected then
          I.hcat [
            I.string A.(fg white ++ bg black ++ st bold ++ st blink) "▶ ";
            I.string A.(fg white ++ bg black ++ st bold) (
              unit_type_str u ^ " (" ^ (string_of_int (turns_left u))
              ^ (if turns_left u = 1 then " turn)" else " turns)")
            )
          ]
          else
          I.string text (
            "  " ^ unit_type_str u ^ " (" ^ (string_of_int (turns_left u))
              ^ (if turns_left u = 1 then " turn)" else " turns)")
          )
          in
        I.vcat [
          snap (I.hcat (List.map (fun t -> tab_img t (t = (City c))) tabs));
          I.void 1 1;
          snap (I.(string text "CURRENT PRODUCTION:"));
          I.void 1 1;
          (
            match current_unit with
            | Some u -> snap (unit_img (-1) u true false)
            | None -> snap I.(string text "Choose a Unit to build")
          );
          I.void 1 2;
          snap (I.string text "BUILDABLE UNITS:");
          I.void 1 1;
          snap (
            I.hsnap ~align:`Left (pane_width - 8) (
              match current_unit with
              | Some curr ->
              I.vcat (List.mapi (fun i u -> unit_img i u (u = curr) false) units)
              | None -> I.vcat (List.mapi (fun i u -> unit_img i u true true) units)
            )
          );
          I.void 1 2;
          snap (I.string text "Select Unit with [J] and [K]");
          I.void 1 1;
          snap (I.string text "Confirm with [SPACE]");
        ]
      | None -> empty_city
    end
  | Tech t_index ->
    let tabs = [Unit (0, 0); City 0; Tech t_index] in
    let current_tech = Player.current_tech player in
    let techs = State.available_techs gst in
    let turns_left t =
      if Player.science_rate player = 0 then 99
      else
        max 1 (1 + ((Tech.tech_cost t - Player.science player)
        / (Player.science_rate player))) in
    let tech_img i t show show_selected blink =
      let selected_text = A.(fg white ++ bg black ++ st bold) in
      if not show || List.length techs = 0 then I.empty else
      if i = t_index %! List.length techs && show_selected then
      I.hcat [
        I.string (if blink then A.(selected_text ++ st blink) else selected_text) "▶ ";
        I.string selected_text (
          tech_str t ^ " (" ^ (string_of_int (turns_left t)) ^ " turns)"
        )
      ]
      else
      I.string text (
        "  " ^ tech_str t ^ " (" ^ (string_of_int (turns_left t)) ^ " turns)"
      )
    in
    I.vcat [
      snap (I.hcat (List.map (fun t -> tab_img t (t = (Tech t_index))) tabs));
      I.void 1 1;
      snap (I.(string text "CURRENT RESEARCH:"));
      I.void 1 1;
      snap (
        match current_tech with
        | Some t -> tech_img (0) t true true false
        | None -> snap I.(string text "Choose research")
      );
      I.void 1 2;
      snap (I.string text "AVAILABLE TECHS:");
      I.void 1 1;
      snap (
        snap (
          match current_tech with
          | Some curr ->
            I.vcat (List.mapi (fun i t -> tech_img i t (t != curr) false true) techs)
          | None -> I.vcat (List.mapi (fun i t -> tech_img i t true true true) techs)
        )
      );
      I.void 1 2;
      snap (I.string text "Select Tech with [J] and [K]");
      snap (I.string text "Confirm with [SPACE]");
      I.void 1 2;
      snap (I.string text "RESEARCHED TECHS:");
      snap (
        I.vcat (
          let map i t = tech_img i t true false false in
          List.mapi map (Player.techs player)
        )
      )
    ]
  in
  I.zcat [
    pane_content;
    I.(char A.(fg white ++ bg black) ' ' pane_width (h - top_padding))
  ] |> I.pad ~t:top_padding

(* [ui_img (w, h) gst] is the image with the status bar, player bar, and left pane *)
let ui_img (w, h) gst =
  I.zcat [
    status_bar (w, h) gst;
    player_bar (w, h) gst;
    left_pane (w, h) gst
  ]

(* [calculate_tiles_w_h (w, h)] returns the number of tiles that can be displayed
 * given the dimensions [(w, h)], in the form (tiles_across, tiles_down) *)
let calculate_tiles_w_h (w, h) =
  let tiles_w = (float_of_int w -. float_of_int (pane_width w) -.
                float_of_int left_padding -.
                float_of_int right_padding -. (float_of_int tile_width *. 1.5)) /.
                float_of_int tile_width |> floor |> int_of_float in
  let tiles_h = (float_of_int h -. float_of_int top_padding -.
                float_of_int bottom_padding -. (float_of_int tile_height /. 2.)) /.
                float_of_int tile_height |> floor |> int_of_float in
  (tiles_w, tiles_h)

(* [terrain_img tile] returns the image for [tile]'s terrain *)
let terrain_img tile =
  World.(
    if elevation tile = Peak then I.empty else
    match terrain tile with
    | Grassland -> I.string A.(fg green) ",,,,,,,,,,,,,,"
    | Plains -> I.string A.(fg yellow) ",,,,,,,,,,,,,,"
    | Desert -> I.string A.(fg lightyellow) "______________"
    | Tundra -> I.string A.(fg (gray 13)) "-=--=-=--=-==-"
    | Ice -> I.string A.(fg white) "〜〜〜〜〜〜〜〜〜〜〜〜〜〜"
    | Ocean -> I.string A.(fg blue) "〜〜〜〜〜〜〜〜〜〜〜〜〜〜"
    | Coast -> I.(hcat [
      string A.(fg lightyellow) "〜〜-.";
      string A.(fg blue) "______";
      string A.(fg lightyellow) ".-〜〜";
    ])
    | Lake -> I.string A.(fg blue) "--------------")

(* [feature_img f] returns the image for the given feature *)
let feature_img f =
  World.(
    match f with
    | Forest -> I.string A.(fg green) " ↟  ↟ ↟↟ ↟  ↟ "
    | Jungle -> I.string A.(fg lightgreen) "↟↟↟↟↟↟↟↟↟↟↟↟"
    | Oasis -> I.string A.(fg blue) "_↟___↟___↟__")

(* [feature_img tile] returns the image for [tile]'s feature, if present *)
let feature_opt_img tile =
  World.(
    match feature tile with
    | Some f -> feature_img f
    | None -> I.empty)

(* [elevation_img u tile] returns the image for [tile]'s elevation,
 * where [u] indicates whether the text should be the highlighted color *)
let elevation_img u tile =
  let str =
    World.(
      match elevation tile with
      | Flatland -> "            "
      | Hill -> "︵  ︵︵ ︵ ︵ ︵︵"
      | Peak -> "△^△^△^^△^△^△^△"
    ) in
  I.string A.(fg (if u then blue else (gray 7)) ++ st underline) str

(* [resource_img r] returns the image for resource [r] *)
let resource_img r =
  World.(
    match r with
    | Fish -> I.string A.empty "Fish"
    | Crab -> I.string A.empty "Crab"
    | Gold -> I.string A.(fg yellow) "Gold"
    | Silver -> I.string A.(fg (gray 10)) "Silver"
    | Gems -> I.string A.(fg lightmagenta) "Gem"
    | Salt -> I.string A.empty "Salt"
    | Iron -> I.string A.(fg (gray 10)) "Iron"
    | Marble -> I.string A.(fg (gray 10)) "Marble"
    | Stone -> I.string A.(fg (gray 10)) "Stone"
    | Wheat -> I.string A.(fg lightyellow) "Wheat"
    | Corn -> I.string A.(fg yellow) "Corn"
    | Rice -> I.string A.empty "Rice"
    | Furs -> I.string A.empty "Furs"
    | Ivory -> I.string A.empty "Ivory"
    | Deer -> I.string A.empty "Deer"
    | Sheep -> I.string A.empty "Sheep"
    | Cattle -> I.string A.empty "Cattle"
    | Horses -> I.string A.empty "Horses"
    | Cotton -> I.string A.empty "Cotton"
    | Banana -> I.string A.(fg yellow) "Bananas"
    | Sugar -> I.string A.empty "Sugar"
  )

(* [resource_opt_img r] returns the image for [tile]'s resource, if present *)
let resource_opt_img gst tile =
  World.(
    match visible_resource gst tile with
    | Some r -> resource_img r
    | None -> I.empty)

(* [tile_unit_str units] is the string indicating the number of units on a tile *)
let tile_unit_str units =
  if List.length units = 0 then ""
  else if List.length units = 1 then string_of_int (List.length units) ^ " unit"
  else string_of_int (List.length units) ^ " units"

(* [city_imgs (col, row) gst] returns the image for the city on the map *)
let city_imgs (col, row) gst =
  match State.city (col, row) gst with
  | Some (p, city) ->
    let text s = I.string A.(fg (gray 3)) s in
    let white_text s = I.string A.(fg white ++ st bold) s in
    let pop_attr = A.(fg green ++ st bold) in
    let prod_attr = A.(fg yellow ++ st bold) in
    let top =
      let health = A.(fg red) in
      I.(hsnap 12 (hcat [
        I.void 1 1;
        I.uchar health 9829 1 1;
        I.void 1 1;
        I.string health (string_of_int (Entity.health (Entity.City city)));
        I.void 2 1;
        (
          let str = if gst.players.(gst.current_player) = p
            then ("YOU")
            else ("P" ^ string_of_int (State.player_number gst p)) in
            I.string (player_color gst p) str
        )
      ])) in
    let middle =
      let pop = Entity.population city in
      let pop_frac = float_of_int (Entity.food_stock city)
        /. (float_of_int (Entity.growth_req pop)) in
      let pop_text =
        I.string pop_attr (progress_str pop_frac) in
      let prod_frac =
        match Entity.unit_production city with
        | Some u ->
          float_of_int (Entity.production_stock city)
          /. float_of_int (Entity.unit_cost u)
        | None -> 0.0
      in
      let prod_text =
        I.string prod_attr (progress_str prod_frac) in
      I.(
        hsnap 14 (hcat [
          string pop_attr (string_of_int pop);
          pop_text;
          text " ❰";
          white_text "CITY";
          text "❱ ";
          prod_text;
          I.void 1 1;
        ])
      )
    in
    let bottom = text "▐▐▗^▙▗▎▙^▟▗▐▟▐^▙▗▐" in
    let below = text "" in
    (top, middle, bottom, below)

  | None -> (I.empty, I.empty, I.empty, I.empty)

(* [move_unit_tile gst dir] returns a new game state for movement in a direction,
 * if the move is a legal move *)
let move_unit_tile gst dir =
  let (max_cols, max_rows) = World.map_dimensions gst.map in
  let (col, row) = gst.selected_tile in
  match dir with
  | `TopLeft ->
    if col - 1 >= 0 then
      if (col mod 2 = 1) then (col - 1, row)
      else if row - 1 >= 0 then (col - 1, row - 1)
      else (col - 1, row)
    else (col, row)
  | `TopMiddle -> if row - 1 >= 0 then (col, row - 1) else (col, row)
  | `TopRight ->
    if col + 1 < max_cols then
      if (col mod 2 = 1) then (col + 1, row)
      else if row > 0 then (col + 1, row - 1)
      else (col, row)
    else (col, row)
  | `BottomLeft ->
    if col - 1 >= 0 && row + 1 < max_rows then
      if (col mod 2 = 1) then
        (col - 1, row + 1)
      else (col - 1, row)
    else (col, row)
  | `BottomMiddle -> if row + 1 < max_rows then (col, row + 1) else (col, row)
  | `BottomRight ->
    if col + 1 < max_cols && row + 1 < max_rows then
      if (col mod 2 = 1) then
        (col + 1, row + 1)
      else (col + 1, row)
    else (col, row)

(* [tile_img is_selected (col, row) (left_col, top_row) gst (w, h)] is the image
 * for the tile in position [(col, row)] *)
let tile_img is_selected (col, row) (left_col, top_row) gst (w, h) =
  let color = if is_selected then A.(fg blue) else A.(fg (gray 3)) in
  let text_color = A.(fg white) in
  let color_underline = A.(color ++ st underline) in
  let odd_even_col_offset = if col mod 2 = 1 then (tile_height/2) else 0 in
  let top_underline_offset = if row = top_row || is_selected then 0 else 1 in
  let left = initial_tile_left w + (tile_width*(col - left_col)) in
  let top = initial_tile_top + (tile_height*(row - top_row)) + odd_even_col_offset + top_underline_offset in
  let t = World.get_tile gst.map col row in
  let units = State.units (col, row) gst in
  let (city_top, city_mid, city_bot, city_bel) = city_imgs (col, row) gst in
  grid [
    if row = top_row then [I.void 5 1; I.string color_underline "            "]
    else if is_selected then
      let (above_col, above_row) = move_unit_tile gst `TopMiddle in
      [I.void 5 1; I.hsnap 12 (elevation_img true (World.get_tile gst.map above_col above_row))]
    else [];
    [I.void 4 1; I.uchar color 0x2571 1 1; I.hsnap 12 city_top; I.uchar color 0x2572 1 1];
    [I.void 3 1; I.uchar color 0x2571 1 1; I.hsnap 14 city_mid; I.uchar color 0x2572 1 1];
    [I.void 2 1; I.uchar color 0x2571 1 1; I.hsnap 16 city_bot; I.uchar color 0x2572 1 1];
    [I.void 1 1; I.uchar color 0x2571 1 1; I.hsnap 18 city_bel; I.uchar color 0x2572 1 1];
    [I.uchar color 0x2571 1 1; I.hsnap 20 (I.string text_color (tile_unit_str units)); I.uchar color 0x2572 1 1];
    [I.uchar color 0x2572 1 1; I.hsnap 20 I.(I.string text_color (improvement_opt_str t)); I.uchar color 0x2571 1 1];
    [I.void 1 1; I.uchar color 0x2572 1 1; I.hsnap 18 (resource_opt_img gst t); I.uchar color 0x2571 1 1];
    [I.void 2 1; I.uchar color 0x2572 1 1; I.hsnap 16 (feature_opt_img t); I.uchar color 0x2571 1 1];
    [I.void 3 1; I.uchar color 0x2572 1 1; I.hsnap 14 (terrain_img t); I.uchar color 0x2571 1 1];
    [I.void 4 1; I.uchar color 0x2572 1 1; I.hsnap 12 (elevation_img is_selected t); I.uchar color 0x2571 1 1];
  ] |> I.pad ~l:left ~t:top

(* [game_map (w, h) gst] returns the image with the game map of tiles drawn *)
let game_map (w, h) gst =
  let rec game_map_helper img (w, h) gst tiles_w tiles_h (col, row) (left_col, top_row) (map_cols, map_rows) =
    let (next_col, next_row) = if col < min (map_cols - 1) (tiles_w + left_col) then (col + 1, row) else (left_col, row+1) in
    let acc = I.(img </> tile_img false (col, row) (left_col, top_row) gst (w, h)) in
    if next_row <= min (map_rows - 1) (tiles_h + top_row - 1) then
      game_map_helper acc (w, h) gst tiles_w tiles_h (next_col, next_row) (left_col, top_row) (map_cols, map_rows)
    else acc in
  let (tiles_w, tiles_h) = calculate_tiles_w_h (w, h) in
  let (selected_col, selected_row) = gst.selected_tile in
  let (left_col, top_row) = gst.map_display in
  let (map_cols, map_rows) = World.map_dimensions gst.map in
  I.(
    pad ~t:(h-1) (hsnap ~align:`Right (w-4) (tile_yields_img (World.get_tile gst.map selected_col selected_row)))
    </>
    game_map_helper (tile_img true (selected_col, selected_row) (left_col, top_row) gst (w, h)) (w, h) gst tiles_w tiles_h (left_col, top_row) (left_col, top_row) (map_cols, map_rows)
  )

(* [img t (w, h) gst] returns the image of the menu bars and game map *)
let img t (w, h) gst =
  I.(ui_img (w, h) gst </> game_map (w, h) gst)

(* [select_tile direction gst] returns the new coordinates after selecting a new tile *)
let select_tile direction gst =
  let map = State.game_map gst in
  let (max_cols, max_rows) = World.map_dimensions map in
  let (current_col, current_row) = gst.selected_tile in
  match direction with
  | `Up -> (current_col, max 0 (current_row - 1))
  | `Down -> (current_col, min (max_rows - 1) (current_row + 1))
  | `Left -> (max 0 (current_col - 1), current_row)
  | `Right -> (min (max_cols - 1) (current_col + 1), current_row)

(* [change_pane_state up pst tile_changing is_improvement_key] handles changing the left pane,
 * either to a new screen or a unit, tech, etc. selection *)
let change_pane_state up pst tile_changing is_improvement_key =
  let diff = if up then 1 else -1 in
  match pst with
  | Unit (u, i) ->
    if tile_changing then Unit (0,0)
    else if is_improvement_key then Unit (u, i + diff)
    else Unit (u + diff, i)
  | City c -> if tile_changing then City (0) else City (c + diff)
  | Tech t -> if tile_changing then Tech (0) else Tech (t + diff)

(* [move_unit gst dir] returns a new game state with the current selected unit being moved *)
let move_unit gst dir =
  match gst.pane_state with
  | Unit (u,_) ->
    let (col, row) = gst.selected_tile in
    let units = State.unit_refs (col,row) gst |> List.split |> snd in
    let num_units = List.length units in
    if num_units > 0 then
      let current_unit = List.nth units (u %! num_units) in
      let (new_col, new_row) = move_unit_tile gst dir in
      if (col, row) <> (new_col, new_row) && Player.player_owns_entity (gst.players.(gst.current_player)) current_unit then
        let gst', success = State.make_move gst current_unit (World.get_tile gst.map new_col new_row) in
        if not success then gst' else { gst' with
          selected_tile = (new_col, new_row)
        }
      else gst
    else gst
  | _ -> gst

(* [found_city gst] returns a new game state with a new city founded at the current
 * selected tile, if possible according to game rules *)
let found_city gst =
  match gst.pane_state with
  | Unit (u,_) ->
    let (col, row) = gst.selected_tile in
    let tile = World.get_tile gst.map col row in
    let units = State.unit_refs (col,row) gst in
    let num_units = List.length units in
    if num_units > 0 then
      let current_unit_num = u %! num_units in
      let _, current_unit = List.nth units current_unit_num in
      let current_player = gst.players.(gst.current_player) in
      let unit_entity = Entity.get_unit_entity !current_unit in
      if Entity.unit_type unit_entity = Entity.Worker
        && Player.player_owns_entity current_player current_unit
        && Entity.moves_left unit_entity > 0
      then State.found_city gst tile current_unit
      else gst
    else gst
  | _ -> gst

(* [build_improvement gst] returns a new game state with an improvement built
 * on the selected tile, if possible *)
let build_improvement gst =
  match gst.pane_state with
  | Unit (u, i) ->
    let (col, row) = gst.selected_tile in
    let tile = World.get_tile gst.map col row in
    let units = State.unit_refs (col,row) gst in
    let num_units = List.length units in
    if num_units > 0 then
      let current_unit_num = u mod num_units in
      let current_unit = snd (List.nth units current_unit_num) in
      let current_player = gst.players.(gst.current_player) in
      if Entity.unit_type (Entity.get_unit_entity !current_unit) = Entity.Worker &&
        Player.player_owns_entity current_player current_unit then
        let possible_improvements = possible_improvements gst tile in
        let num_possible_improvements = List.length possible_improvements in
        if num_possible_improvements > 0 then
          let num_current_improvement = i mod num_possible_improvements in
          let new_improvement = List.nth possible_improvements num_current_improvement in
          let health = Entity.health !current_unit in
          current_unit := Entity.set_health !current_unit (health - 50);
          World.set_improvement gst.map col row new_improvement; gst
        else gst
      else gst
    else gst
  | _ -> gst

(* [main t gst] handles changes in the game state based on player input *)
let rec main t gst =
  let (w, h) = Term.size t in
  Term.image t (img t (w, h) gst);
  match Term.event t with
  | `End | `Key (`Uchar 68, [`Ctrl]) | `Key (`Uchar 67, [`Ctrl])
  | `Key (`Escape, []) -> Quit
  | `Key (`Arrow direction, []) ->
    let (tiles_w, tiles_h) = calculate_tiles_w_h (w, h) in
    let (new_selected_col, new_selected_row) = select_tile direction gst in
    let (current_left_col, current_top_row) = gst.map_display in
    let new_map_display =
      if new_selected_col > (current_left_col + tiles_w) then
        (current_left_col + 2, current_top_row)
      else if new_selected_row > (current_top_row + tiles_h - 1) then
        (current_left_col, current_top_row + 1)
      else if new_selected_col < current_left_col then
        (current_left_col - 2, current_top_row)
      else if new_selected_row < current_top_row then
        (current_left_col, current_top_row - 1)
      else (current_left_col, current_top_row) in
    let new_gst = { gst with
      selected_tile = select_tile direction gst;
      map_display = new_map_display;
      pane_state = change_pane_state true gst.pane_state true false
    } in
    main t new_gst
  | `Resize (nw, nh) -> main t gst
  | `Key (`Enter, []) -> main t (State.next_turn gst)
  | `Key (`Uchar 49, []) -> main t {gst with pane_state = Unit (0,0)}
  | `Key (`Uchar 50, []) -> main t {gst with pane_state = City 0}
  | `Key (`Uchar 51, []) -> main t {gst with pane_state = Tech 0}
  | `Key (`Uchar 106, []) -> main t {gst with pane_state = change_pane_state true gst.pane_state false false}
  | `Key (`Uchar 107, []) -> main t {gst with pane_state = change_pane_state false gst.pane_state false false}
  | `Key (`Uchar 91, []) -> main t {gst with pane_state = change_pane_state true gst.pane_state false true}
  | `Key (`Uchar 93, []) -> main t {gst with pane_state = change_pane_state false gst.pane_state false true}
  | `Key (`Uchar 32, []) ->
    let player = gst.players.(gst.current_player) in
    begin
      match gst.pane_state with
      | Tech i ->
        if Player.current_tech player = None then (
          let techs = State.available_techs gst in
          if List.length techs > 0 then
          (let tech = List.nth techs (i %! List.length techs) in
          gst.players.(gst.current_player) <- Player.research_tech (gst.players.(gst.current_player)) tech)
        );
        main t gst
      | City i ->
        let city_ref = State.city_ref gst.selected_tile gst in
        begin
          match city_ref with
          | Some e ->
            let entity = snd e in
            if List.mem entity (Player.entities player) then
            Entity.(
              match !entity with
              | City city ->
                if Entity.unit_production city = None then (
                  let prods = State.available_units gst in
                  let prod = List.nth prods (i %! List.length prods) in
                  Entity.change_production entity prod
                );
                main t gst
              | _ -> main t gst
            ) else main t gst
          | None ->
          main t gst
        end
      | _ -> main t gst
    end
  | `Key (`Uchar 113, []) -> main t (move_unit gst `TopLeft)
  | `Key (`Uchar 119, []) -> main t (move_unit gst `TopMiddle)
  | `Key (`Uchar 101, []) -> main t (move_unit gst `TopRight)
  | `Key (`Uchar 97, []) -> main t (move_unit gst `BottomLeft)
  | `Key (`Uchar 115, []) -> main t (move_unit gst `BottomMiddle)
  | `Key (`Uchar 100, []) -> main t (move_unit gst `BottomRight)
  | `Key (`Uchar 102, []) -> main t (found_city gst)
  | `Key (`Uchar 105, []) -> main t (build_improvement gst)
  | _ -> main t gst

let new_state t gst =
  main t gst