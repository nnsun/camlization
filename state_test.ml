open OUnit2
open State

let num_players = 4
let init = initial_game_state { player_count = num_players }

let init_gold p = Player.gold p <> 0
let init_techs p = Player.techs p <> []
let init_sci p = Player.science p <> 0
let init_unit p = List.length (Player.entities p) <> 1
let init_worker p = Entity.unit_type
    (Entity.get_unit_entity (!(List.hd (Player.entities p)))) <> Entity.Worker



let tests = [
  "init_turns" >:: (fun _ -> assert_equal init.player_turns 0);
  "num_players" >:: (fun _ -> assert_equal (Array.length init.players) num_players);
  "init_gold" >:: (fun _ -> assert_equal (Array.exists init_gold init.players) false);
  "init_techs" >:: (fun _ -> assert_equal (Array.exists init_techs init.players) false);
  "init_sci" >:: (fun _ -> assert_equal (Array.exists init_techs init.players) false);
  "init_unit" >:: (fun _ -> assert_equal (Array.exists init_unit init.players) false);
  "init_worker" >:: (fun _ -> assert_equal (Array.exists init_unit init.players) false);
]

let suite = "test_suite" >::: tests

let _ = run_test_tt_main suite