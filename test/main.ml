open OUnit2
open Rummikaml
open Model

let board_tests = [ ("testing testy test" >:: fun _ -> assert_equal [] []) ]
let game_tests = []

let suite =
  "test suite for rummikaml" >::: List.flatten [ board_tests; game_tests ]

let () = run_test_tt_main suite
