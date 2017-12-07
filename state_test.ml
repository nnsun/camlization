open OUnit2
open State

let init = initial_game_state { player_count = 4 }

let tests = [
]

let suite = "test_suite" >::: tests

let _ = run_test_tt_main suite