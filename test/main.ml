open OUnit2
open Rummikaml
open Model
open View

let joker = Joker
let t1 = Num { color = Yellow; num = 2 }
let t2 = Num { color = Black; num = 13 }

let s1 = [Num{color = Red; num = 5};
Num{color = Red; num = 6}; Num{color = Red; num = 7}]

let board1  = [[t1; t2]]

let board2 = [[Num{color = Red; num = 5}]; s1; 
[Num{color = Yellow; num = 11}; Num{color = Yellow; num = 12}; 
Num{color = Yellow; num =13}]]

let board3 = [[Num{color = Blue; num = 7}; Num{color = Red; num = 7}; 
Num{color = Yellow; num = 7}]; [Num{color = Yellow; num = 2}]; s1]

let board4 = [[Num{color = Yellow; num = 1}; Num{color = Yellow; num = 2}; 
Num{color = Yellow; num = 3}]; [Num{color = Yellow; num = 10}; 
Num{color = Yellow; num = 11}; Num{color = Yellow; num = 12}; 
Num{color = Yellow; num = 13}]]

let board5 = [[Num{color = Blue; num = 2}; Num{color = Yellow; num = 2};
Num{color = Red; num = 2}]; [Num{color = Red; num = 12}; 
Num{color = Blue; num = 12}; Num{color = Black; num = 12}]]

let board6 = [[Num{color = Red; num = 8}; Num{color = Red; num = 9}; Joker]; 
[Num{color = Blue; num = 10}; Num{color = Red; num = 10}; 
Num{color = Yellow; num = 10}]]

let board7 = [[Num{color = Red; num = 10}; Joker; Joker]]

let board8 = [[Num{color = Yellow; num = 10}; Joker; 
Num{color = Yellow; num = 12}; Joker]]

let board9 = [[Num{color = Red; num = 4}; Num{color = Blue; num = 4}; 
Num{color = Yellow; num = 4}; Num{color = Black; num = 4}; 
Num{color = Yellow; num = 4}]]

module Printer = CLIPrinter (Game)

let board_tests = [ ("testing testy test" >:: fun _ -> assert_equal [] []);
  
  ("not enough tiles in board" >:: fun _ -> assert_equal false 
  (Board.check board1));

  ("not enough tiles in set" >:: fun _ -> assert_equal false 
  (Board.check board2));

  ("not enough tiles middle of board" >:: fun _ -> assert_equal false 
  (Board.check board3));

  ("simple same colors (2 sets)" >:: fun _ -> assert_equal true 
  (Board.check board4));

  ("simple same numbers (2 sets)" >:: fun _ -> assert_equal true
  (Board.check board5));

  ("simple one set each type" >:: fun _ -> assert_equal true
  (Board.check board6));

  ("one set two jokers" >:: fun _ -> assert_equal true
  (Board.check board6));

  ("scattered jokers same color" >:: fun _ -> assert_equal true
  (Board.check board8));

  ("too many tiles with same num" >:: fun _ -> assert_equal false
  (Board.check board9));

]
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
