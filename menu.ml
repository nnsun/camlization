open Notty
open Notty_unix
open Notty_helper
open State

let img s = I.string A.empty s

let title_width = 67
let title_height = 8
let title_img =
  let v_width = 15 in
  let v_strs = [
    "8b           d8";
    "`8b         d8'";
    " `8b       d8'";
    "  `8b     d8'";
    "   `8b   d8'";
    "    `8b d8'";
    "     `888'";
    "      `8'"
  ] in
  let v_str_imgs = List.map (fun s -> I.string A.(fg yellow) s) v_strs in
  let v_void = I.void ((title_width - v_width) / 2) title_height in
  let v_img = I.(v_void <|> (I.vcat v_str_imgs) <|> v_void) in

  (*
   *  #####
   * #     #   ##   #    # #      # ######   ##   ##### #  ####  #    #
   *#         #  #  ##  ## #      #    ##   #  #    #   # #    # ###  #
   * #     # # ## # # ## # #      #  ##    # ## #   #   # #    # #  ###
   *  #####  #    # #    # ###### # ###### #    #   #   #  ####  #    #
   *)
  (* Add void spaces between LIZA characters *)

  let line1 = I.hcat [
    img "  #####";
    I.void (title_width - 8) 1
  ] in
  let line2 = I.hcat [
    img " #     #   ##   #    # #";
    I.void 6 1;
    img "#";
    I.void 1 1;
    img "######";
    I.void 3 1;
    img "##";
    I.void 3 1;
    img "##### #  ####  #    #"
  ] in
  let line3 = I.hcat [
    img "#         #  #  ##  ## #";
    I.void 6 1;
    img "#";
    I.void 4 1;
    img "##";
    I.void 3 1;
    img "#";
    I.void 2 1;
    img "#";
    I.void 4 1;
    img "#   # #    # ###  #"
  ] in
  let line4 = I.hcat [
    img " #     # # ## # # ## # #";
    I.void 6 1;
    img "#";
    I.void 2 1;
    img "##";
    I.void 4 1;
    img "#";
    I.void 1 1;
    img "##";
    I.void 1 1;
    img "#";
    I.void 3 1;
    img "#   # #    # #  ###";
  ] in
  let line5 = I.hcat [
    img "  #####  #    # #    # ######";
    I.void 1 1;
    img "#";
    I.void 1 1;
    img "######";
    I.void 1 1;
    img "#";
    I.void 4 1;
    img "#";
    I.void 3 1;
    img "#   #  ####  #    #";
  ] in
  let camlization_lines = [line1; line2; line3; line4; line5] in
  let caml = center (I.vcat camlization_lines) title_width title_height in
  I.(caml </> v_img)

let loading_height = 3
let loading_img =
  I.vcat [
    img "╦  ╔═╗╔═╗╔╦╗╦╔╗╔╔═╗";
    img "║  ║ ║╠═╣ ║║║║║║║ ╦";
    img "╩═╝╚═╝╩ ╩═╩╝╩╝╚╝╚═╝";
  ]

type menu_item = {
  index: int;
  img: image;
  enter_state: State.state
}

let copyright_items = [|
  {
    index = -1;
    img = I.vcat [
      I.void 1 1;
      img "© 2017 Alexander Strandberg, Daniel Li, Cynthia Tu, and Ning Ning Sun.";
      img "Developed for CS 3110 Final Project in the fall of 2017. The ratings";
      img "icon is a trademark of the Entertainment Software Association. All";
      img "other marks and trademarks are the property of their respective owners.";
      img "All rights reserved. The content of this videogame is fictional and is";
      img "not intended to represent or depict an actual record of the events,";
      img "persons or entities in the game's historical setting.";
      I.void 1 1
    ];
    enter_state = Menu (Copyright)
  };
  {
    index = 1;
    img = I.(I.void 1 1 <-> img "Press Enter to Continue" <-> I.void 1 1);
    enter_state = Menu (Main 0)
  };
|]

let main_menu_items = [|
  {
    index = 0;
    img = I.vcat [
      I.void 1 2;
      img "╔╦╗╦ ╦╦ ╔╦╗╦╔═╗╦  ╔═╗╦ ╦╔═╗╦═╗";
      img "║║║║ ║║  ║ ║╠═╝║  ╠═╣╚╦╝║╣ ╠╦╝";
      img "╩ ╩╚═╝╩═╝╩ ╩╩  ╩═╝╩ ╩ ╩ ╚═╝╩╚═";
      I.void 1 2;
    ];
    enter_state = Menu (Multiplayer (0, { player_count = 2 }))
  };
  {
    index = 1;
    img = I.vcat [
      I.void 1 2;
      img "╔═╗╔═╗╔╦╗╦╔═╗╔╗╔╔═╗";
      img "║ ║╠═╝ ║ ║║ ║║║║╚═╗";
      img "╚═╝╩   ╩ ╩╚═╝╝╚╝╚═╝";
      I.void 1 2;
    ];
    enter_state = Menu (Main 0)
  };
  {
    index = 2;
    img = I.vcat [
      I.void 1 2;
      img "╔═╗╔═╗╦╔═╔╗╔╔═╗╦ ╦╦  ╔═╗╔╦╗╔═╗╔╦╗╔═╗╔╗╔╔╦╗╔═╗";
      img "╠═╣║  ╠╩╗║║║║ ║║║║║  ║╣  ║║║ ╦║║║║╣ ║║║ ║ ╚═╗";
      img "╩ ╩╚═╝╩ ╩╝╚╝╚═╝╚╩╝╩═╝╚═╝═╩╝╚═╝╩ ╩╚═╝╝╚╝ ╩ ╚═╝";
      I.void 1 2;
    ];
    enter_state = Menu (Main 0)
  };
  {
    index = 3;
    img = I.vcat [
      I.void 1 2;
      img "╔═╗ ╦ ╦╦╔╦╗";
      img "║═╬╗║ ║║ ║ ";
      img "╚═╝╚╚═╝╩ ╩ ";
      I.void 1 2;
    ];
    enter_state = Quit
  };
|]

let multiplayer_items (i, options) = [|
  {
    index = 0;
    img = I.vcat [
      I.void 1 2;
      img "╔═╗╔╦╗╔═╗╦═╗╔╦╗  ╔═╗╔═╗╔╦╗╔═╗";
      img "╚═╗ ║ ╠═╣╠╦╝ ║   ║ ╦╠═╣║║║║╣ ";
      img "╚═╝ ╩ ╩ ╩╩╚═ ╩   ╚═╝╩ ╩╩ ╩╚═╝";
      I.void 1 2;
    ];
    enter_state = Game (State.initial_game_state options)
  };
  {
    index = 1;
    img = I.vcat [
      I.void 1 2;
      img "╔═╗╦  ╔═╗╦ ╦╔═╗╦═╗╔═╗";
      img "╠═╝║  ╠═╣╚╦╝║╣ ╠╦╝╚═╗";
      img "╩  ╩═╝╩ ╩ ╩ ╚═╝╩╚═╚═╝";
      I.void 1 1;
      I.hsnap 20 (img (" ❮ " ^ string_of_int options.player_count ^ " ❯"));
      I.void 1 2;
    ];
    enter_state = Menu (Multiplayer (i, options))
  };
  {
    index = 2;
    img = I.vcat [
      I.void 1 2;
      img "╔╗ ╔═╗╔═╗╦╔═";
      img "╠╩╗╠═╣║  ╠╩╗";
      img "╚═╝╩ ╩╚═╝╩ ╩";
      I.void 1 2;
    ];
    enter_state = Menu (Main 0)
  }
|]

let menu_img (items: menu_item array) index =
  let imgs = Array.map (fun i -> i.img) items in
  let width = 6 + Array.fold_left (fun m i -> max m (I.width i)) 0 imgs in
  let height =
    Array.fold_left (fun a i -> a + I.height i) (Array.length items) imgs - 1 in

  let vbar = I.hcat [
    I.uchar A.empty 0x2551 1 height;
    I.void 1 height;
    I.uchar A.empty 0x2551 1 height;
  ] in
  let item_img item =
    let w = width in
    let fill_img =
      if item.index = index then
      border A.(fg yellow) 0x2591 w (I.height item.img)
      else I.empty in
    I.vcat [
      I.(I.hsnap w item.img </> fill_img);
      I.hsnap width (I.uchar A.empty 0x2500 (width - 8) 1);
    ]
    in
  let items_imgs = Array.map item_img items |> Array.to_list |> I.vcat in
  let hbar = I.uchar A.empty 0x2550 width 1 in
  let centered_items = I.vsnap height (I.crop ~b:1 items_imgs) in
  I.vcat [
    I.(img "╔══" <|> hbar <|> img "══╗");
    I.(img "║ ╔" <|> hbar <|> img "╗ ║");
    I.(vbar <|> centered_items <|> vbar);
    I.(img "║ ╚" <|> hbar <|> img "╝ ║");
    I.(img "╚══" <|> hbar <|> img "══╝");
  ]

let img t (w, h) mst =
  match mst with
  | Loading -> I.vcat [
      I.pad ~t:(h/3) (I.hsnap w title_img);
      I.void 1 (h - 4 - loading_height - title_height - (h/3));
      I.pad ~b:4 (I.hsnap w loading_img);
    ]
  | Copyright ->
    I.zcat [
      center (menu_img copyright_items 0) w h;
      I.pad ~t:1 ~l:(w - title_width - 2) title_img
    ]
  | Main i ->
    I.zcat [
      center (menu_img main_menu_items i) w h;
      I.pad ~t:1 ~l:(w - title_width - 2) title_img
    ]
  | Multiplayer (i, options) ->
    I.zcat [
      center (menu_img (multiplayer_items (i, options)) i) w h;
      I.pad ~t:1 ~l:(w - title_width - 2) title_img
    ]
  | Options -> center (I.string A.(fg lightwhite) "Options") w h
  | About -> center (I.string A.(fg lightwhite) "About") w h

let rec copyright t (w, h) mst =
  Term.image t (img t (w, h) mst);
  match Term.event t with
  | `Key (`Enter, []) -> Main 0
  | `Resize (nw, nh) -> copyright t (nw, nh) mst
  | _ -> copyright t (w, h) mst

let rec multiplayer t (w, h) i options =
  Term.image t (img t (w, h) (Multiplayer (i, options)));
  match Term.event t with
  | `End | `Key (`Uchar 68, [`Ctrl]) | `Key (`Uchar 67, [`Ctrl])
  | `Key (`Escape, []) -> Menu (Main 0)
  | `Key (`Arrow(`Up), []) ->
    let new_index =
      if i-1 = -1 then Array.length (multiplayer_items (i, options)) - 1
      else i - 1
    in
    multiplayer t (w, h) new_index options
  | `Key (`Arrow(`Down), []) ->
    let new_index = (i + 1) mod Array.length (multiplayer_items (i, options)) in
    multiplayer t (w, h) new_index options
  | `Key (`Arrow(`Left), []) ->
    if i = 1 then Menu (Multiplayer (i, {
      player_count = max 2 (options.player_count - 1)
    })) else Menu(Multiplayer (i, options))
  | `Key (`Arrow(`Right), []) ->
    if i = 1 then Menu (Multiplayer (i, {
      player_count = min 69 (options.player_count + 1)
    })) else Menu(Multiplayer (i, options))
  | `Key (`Enter, []) -> (multiplayer_items (i, options)).(i).enter_state
  | `Resize (nw, nh) -> multiplayer t (nw, nh) i options
  | _ -> multiplayer t (w, h) i options

let rec main t i =
  let (w, h) = Term.size t in
  Term.image t (img t (w, h) (Main (i)));
  match Term.event t with
  | `End | `Key (`Uchar 68, [`Ctrl]) | `Key (`Uchar 67, [`Ctrl])
  | `Key (`Escape, []) -> Quit
  | `Key (`Arrow(`Up), []) ->
    main t (if i-1 = -1 then Array.length main_menu_items - 1 else i-1)
  | `Key (`Arrow(`Down), []) ->
    main t ((i+1) mod Array.length main_menu_items)
  | `Key (`Enter, []) -> main_menu_items.(i).enter_state
  | _ -> main t i

let new_state t mst =
  let (w, h) = Term.size t in
  match mst with
  | Loading -> Term.image t (img t (w, h) mst); Unix.sleep 0; Menu(Copyright)
  | Copyright -> Menu(copyright t (w, h) mst)
  | Main i -> main t i
  | Multiplayer (i, options) -> multiplayer t (w, h) i options
  | Options -> Menu(Options)
  | About -> Menu(About)