open OUnit2
open Rummikaml
open Model
open View

let joker = Joker
let t1 = Num { color = Yellow; num = 2 }
let t2 = Num { color = Black; num = 13 }

module Printer = CLIPrinter (Game)

let board_tests = [ ("testing testy test" >:: fun _ -> assert_equal [] []) ]
let game_tests = []

let printer_tests =
  [
    ( "test string_of_tile on joker" >:: fun _ ->
      assert_equal "[JJ]" (Printer.string_of_tile joker) );
    ( "test string_of_tile on numbered tile 2Y" >:: fun _ ->
      assert_equal "[2Y]" (Printer.string_of_tile t1) );
    ( "test string_of_tile on numbered tile 13K" >:: fun _ ->
      assert_equal "[13K]" (Printer.string_of_tile t2) );
  ]

let suite =
  "test suite for rummikaml" >::: List.flatten [ board_tests; game_tests ]

let () = run_test_tt_main suite
