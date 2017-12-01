open Notty
open Notty_unix
open Notty_helper
open State

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

  let img s = I.string A.empty s in
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
  let img s = I.string A.empty s in
  I.vcat [
    img "╦  ╔═╗╔═╗╔╦╗╦╔╗╔╔═╗";
    img "║  ║ ║╠═╣ ║║║║║║║ ╦";
    img "╩═╝╚═╝╩ ╩═╩╝╩╝╚╝╚═╝";
  ]

let menu_img (items: image list) =
  let width = 4 + List.fold_left (fun m i -> max m (I.width i)) 0 items in
  let height =
    2 + List.fold_left (fun a i -> a + I.height i) (3 * List.length items) items in

  let img s = I.string A.empty s in
  let vbar = I.hcat [
    I.uchar A.empty 0x2551 1 height;
    I.void 1 height;
    I.uchar A.empty 0x2551 1 height;
  ] in
  let item_img item = I.vcat [
    I.hsnap width item;
    I.void 1 1;
    I.hsnap width (I.uchar A.empty 0x2500 (width - 8) 1);
    I.void 1 1
  ] in
  let items_imgs = List.map item_img items |> I.vcat in
  let hbar = I.uchar A.empty 0x2550 width 1 in
  let centered_items = I.vsnap height (I.vcrop 0 3 items_imgs) in
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
    let img s = I.string A.empty s in
    let copyright = I.vcat [
      img "© 2017 Alexander Strandberg, Daniel Li, Cynthia Tu, and Ning Ning Sun.";
      img "Developed for CS 3110 Final Project in the fall of 2017. The ratings";
      img "icon is a trademark of the Entertainment Software Association. All";
      img "other marks and trademarks are the property of their respective owners.";
      img "All rights reserved. The content of this videogame is fictional and is";
      img "not intended to represent or depict an actual record of the events,";
      img "persons or entities in the game's historical setting."
    ] in
    let items = [
      copyright;
      img "Press Enter to Continue"
    ] in
    I.zcat [
      center (menu_img items) w h;
      I.pad ~t:1 ~l:(w - title_width - 2) title_img
    ]
  | Main -> center (I.string A.(fg lightwhite) "Main menu") w h
  | Multiplayer options -> center (I.string A.(fg lightwhite) "Multiplayer") w h
  | Options -> center (I.string A.(fg lightwhite) "Options") w h
  | About -> center (I.string A.(fg lightwhite) "About") w h

let rec copyright t (w, h) mst =
  Term.image t (img t (w, h) mst);
  match Term.event t with
  | `Key (`Enter, []) -> Main
  | `Resize (nw, nh) -> copyright t (nw, nh) mst
  | _ -> copyright t (w, h) mst

let multiplayer t (w, h) options =
  Game { date = -3000; map = ref World.generate_map; map_display = (0,0) }

let rec main t (w, h) mst =
  Term.image t (img t (w, h) mst);
  match Term.event t with
  | `End | `Key (`Uchar 68, [`Ctrl]) | `Key (`Uchar 67, [`Ctrl])
  | `Key (`Escape, []) -> Quit
  | `Key (`Enter, []) -> multiplayer t (w, h) { player_count_menu_open = false; player_count = 4 }
  | `Resize (nw, nh) -> main t (nw, nh) mst
  | _ -> main t (w, h) mst

let new_state t (w, h) mst =
  Term.image t (img t (w, h) mst);
  match mst with
  | Loading -> Unix.sleep 3; Menu(Copyright)
  | Copyright -> Menu(copyright t (w, h) mst)
  | Main -> main t (w, h) mst
  | Multiplayer options -> multiplayer t (w, h) options
  | Options -> Menu(Options)
  | About -> Menu(About)