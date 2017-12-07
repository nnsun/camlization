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
let init_curr_tech p = Player.current_tech p <> None

let next = next_turn init

let player2 = next.players.(1)
let new_player2 = Player.research_tech player2 Tech.Agriculture

let p3_turn = next_turn next

let p1_again = next_turn (next_turn p3_turn)

let tests = [
  "init_turns" >:: (fun _ -> assert_equal init.player_turns 0);
  "num_players" >:: (fun _ -> assert_equal (Array.length init.players) num_players);
  "init_gold" >:: (fun _ -> assert_equal (Array.exists init_gold init.players) false);
  "init_techs" >:: (fun _ -> assert_equal (Array.exists init_techs init.players) false);
  "init_curr_tech" >:: (fun _ -> assert_equal (Array.exists init_curr_tech init.players) false);
  "init_sci" >:: (fun _ -> assert_equal (Array.exists init_techs init.players) false);
  "init_unit" >:: (fun _ -> assert_equal (Array.exists init_unit init.players) false);
  "init_worker" >:: (fun _ -> assert_equal (Array.exists init_worker init.players) false);
  "init_curr_player" >:: (fun _ -> assert_equal init.current_player 0);
  "next_curr_player" >:: (fun _ -> assert_equal next.current_player 1);
  "diff_selected" >:: (fun _ -> assert_equal (init.selected_tile <> next.selected_tile) true);
  "available_techs" >:: (fun _ -> assert_equal (available_techs init) [Tech.Agriculture]);
  "research_ag" >:: (fun _ -> assert_equal (Player.current_tech new_player2) (Some Tech.Agriculture));
  "p3" >:: (fun _ -> assert_equal p3_turn.current_player 2);
  "p3_turn2" >:: (fun _ -> assert_equal p3_turn.player_turns 2);
  "p1_again" >:: (fun _ -> assert_equal p1_again.current_player 0);
  "player_turns" >:: (fun _ -> assert_equal p1_again.player_turns num_players);
]

let suite = "test_suite" >::: tests

let _ = run_test_tt_main suite