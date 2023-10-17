open OUnit2
open Rummikaml
open Model
open View

let joker = Joker
let t1 = Num { color = Yellow; num = 2 }
let t2 = Num { color = Black; num = 13 }

let row1 =
  [
    Num { color = Red; num = 5 };
    Num { color = Red; num = 6 };
    Num { color = Red; num = 7 };
  ]

let row2 =
  [ Num { color = Black; num = 5 }; Joker; Num { color = Yellow; num = 7 } ]

let board1 = [ [ t1; t2 ] ]

let board2 =
  [
    [ Num { color = Red; num = 5 } ];
    row1;
    [
      Num { color = Yellow; num = 11 };
      Num { color = Yellow; num = 12 };
      Num { color = Yellow; num = 13 };
    ];
  ]

let board3 =
  [
    [
      Num { color = Blue; num = 7 };
      Num { color = Red; num = 7 };
      Num { color = Yellow; num = 7 };
    ];
    [ Num { color = Yellow; num = 2 } ];
    row1;
  ]

let board4 =
  [
    [
      Num { color = Yellow; num = 1 };
      Num { color = Yellow; num = 2 };
      Num { color = Yellow; num = 3 };
    ];
    [
      Num { color = Yellow; num = 10 };
      Num { color = Yellow; num = 11 };
      Num { color = Yellow; num = 12 };
      Num { color = Yellow; num = 13 };
    ];
  ]

let board5 =
  [
    [
      Num { color = Blue; num = 2 };
      Num { color = Yellow; num = 2 };
      Num { color = Red; num = 2 };
    ];
    [
      Num { color = Red; num = 12 };
      Num { color = Blue; num = 12 };
      Num { color = Black; num = 12 };
    ];
  ]

let board6 =
  [
    [ Num { color = Red; num = 8 }; Num { color = Red; num = 9 }; Joker ];
    [
      Num { color = Blue; num = 10 };
      Num { color = Red; num = 10 };
      Num { color = Yellow; num = 10 };
    ];
  ]

let board7 = [ [ Num { color = Red; num = 10 }; Joker; Joker ] ]

let board8 =
  [
    [
      Num { color = Yellow; num = 10 };
      Joker;
      Num { color = Yellow; num = 12 };
      Joker;
    ];
  ]

let board8add = 
  [
    [
      Num { color = Yellow; num = 9 };
      Num { color = Yellow; num = 10 };
      Joker;
      Num { color = Yellow; num = 12 };
      Joker;
    ];
  ]

let board9 =
  [
    [
      Num { color = Red; num = 4 };
      Num { color = Blue; num = 4 };
      Num { color = Yellow; num = 4 };
      Num { color = Black; num = 4 };
      Num { color = Yellow; num = 4 };
    ];
  ]

let board10 =
  [
    [
      Num { color = Red; num = 4 };
      Num { color = Blue; num = 4 };
      Num { color = Black; num = 4};
    ]; 
    [
      Num { color = Red; num = 10 };
      Num { color = Red; num = 11 };
      Num { color = Red; num = 12 };
    ]
  ]

let board10add1 =
  [
    [
      Num { color = Red; num = 4 };
      Num { color = Blue; num = 4 };
      Num { color = Yellow; num = 4 };
      Num { color = Black; num = 4};
    ]; 
    [
      Num { color = Red; num = 10 };
      Num { color = Red; num = 11 };
      Num { color = Red; num = 12 };
    ]
  ]

let board10add2 = 
  [
    [
      Num { color = Red; num = 4 };
      Num { color = Blue; num = 4 };
      Num { color = Black; num = 4};
    ]; 
    [
      Num { color = Red; num = 10 };
      Num { color = Red; num = 11 };
      Num { color = Red; num = 12 };
      Num { color = Red; num = 13 };
    ]
  ]

let board11 = 
  [
    [
      Num { color = Yellow; num = 10 };
      Num { color = Yellow; num = 11 };
      Num { color = Yellow; num = 12 };
      Num { color = Yellow; num = 13 };
    ];
  ]

let board12 =
  [
    [
      Num { color = Red; num = 2 };
      Num { color = Red; num = 3 };
      Num { color = Red; num = 4 }; 
    ];
    [
      Num { color = Blue; num = 9 };
      Num { color = Black; num = 9 };
      Num { color = Yellow; num = 9 };
    ]
  ]

let board13 =
  [
    [
      Num { color = Blue; num = 2 };
      Num { color = Yellow; num = 2 };
      Num { color = Red; num = 2 };
    ];
  ]

let board14 = 
  [
    [
      Joker;
      Num { color = Red; num = 10 };
      Joker;
    ]
  ]

let board15 = 
  [
    [
      Num { color = Red; num = 10 };
      Num { color = Red; num = 11 };
      Num { color = Red; num = 13 };
    ];
  ]

let board16 =
  [
    [
      Num { color = Red; num = 10 };
      Num { color = Red; num = 10 };
      Num { color = Blue; num = 10 };
    ];
  ]

let board17 = 
  [
    [
      Num { color = Yellow; num = 9 };
      Joker;
      Num { color = Yellow; num = 10 };
      Num { color = Yellow; num = 12 };
    ]
  ]

module Printer = CLIPrinter

let board_tests =
  [
    (*add tests*)
    ( "tile at front of one row board" >:: fun _ -> 
      assert_equal board8add (Board.add board8 
      (Num{color = Yellow; num = 9}) (0, 0)) );
    ( "tile in first row" >:: fun _ ->
      assert_equal board10add1 (Board.add board10
      (Num{color = Yellow; num = 4}) (0, 2)) );
    ( "tile at end of last row" >:: fun _ -> 
      assert_equal board10add2 (Board.add board10
      (Num{color = Red; num = 13}) (1, 3)) );

    (*check tests*)
    ( "not enough tiles in board" >:: fun _ ->
      assert_equal false (Board.check board1) );
    ( "not enough tiles in set" >:: fun _ ->
      assert_equal false (Board.check board2) );
    ( "not enough tiles middle of board" >:: fun _ ->
      assert_equal false (Board.check board3) );
    ( "simple same colors (2 sets)" >:: fun _ ->
      assert_equal true (Board.check board4) );
    ( "simple same numbers (2 sets)" >:: fun _ ->
      assert_equal true (Board.check board5) );
    ( "simple one set each type" >:: fun _ ->
      assert_equal true (Board.check board6) );
    ( "one set two jokers" >:: fun _ -> 
      assert_equal true (Board.check board6) );
    ( "scattered jokers same color" >:: fun _ ->
      assert_equal true (Board.check board8) );
    ( "too many tiles with same num" >:: fun _ ->
      assert_equal false (Board.check board9) );
    ( "too many tiles with same color" >:: fun _ ->
      assert_equal false (Board.check board16) );
    ( "tiles not off-by-one" >:: fun _ -> 
      assert_equal false (Board.check board15) );
    ( "joker not placed correctly" >:: fun _ -> 
      assert_equal false (Board.check board17) );

    (*check_first tests*)
    ( "simple one row > 30" >:: fun _ ->
      assert_equal true (Board.check_first board11) );
    ( "simple multiple row > 30" >:: fun _ -> 
      assert_equal true (Board.check_first board12) );
    ( "simple board < 30" >:: fun _ ->
      assert_equal false (Board.check_first board13) );
    ( "simple board = 30 with Jokers" >:: fun _ -> 
      assert_equal true (Board.check_first board14) );
    ( "board > 30 but invalid" >:: fun _ -> 
      assert_equal false (Board.check_first board15) );

  ]

let game_tests = []

let model_helper_tests =
  [
    ("test insert on empty list" >:: fun _ -> assert_equal [ 1 ] (insert 1 [] 0));
    ( "test insert, append to list" >:: fun _ ->
      assert_equal [ 1; 2; 3; 4 ] (insert 4 [ 1; 2; 3 ] 3) );
    ( "test insert, prepend to list" >:: fun _ ->
      assert_equal [ 1; 2; 3; 4 ] (insert 1 [ 2; 3; 4 ] 0) );
    ( "test insert, insert element in middle of list" >:: fun _ ->
      assert_equal [ 1; 2; 3; 4 ] (insert 2 [ 1; 3; 4 ] 1) );
    ( "test replace on short list" >:: fun _ ->
      assert_equal [ 1; 5; 3; 4 ] (replace 5 [ 1; 2; 3; 4 ] 1) );
    ( "test replace, replace head" >:: fun _ ->
      assert_equal [ 5; 2; 3; 4 ] (replace 5 [ 1; 2; 3; 4 ] 0) );
    ( "test replace, replace tail" >:: fun _ ->
      assert_equal [ 1; 2; 3; 5 ] (replace 5 [ 1; 2; 3; 4 ] 3) );
    ( "test remove on short list" >:: fun _ ->
      assert_equal (2, [ 1; 3; 4 ]) (remove 1 [ 1; 2; 3; 4 ]) );
    ( "test remove, remove head" >:: fun _ ->
      assert_equal (1, [ 2; 3; 4 ]) (remove 0 [ 1; 2; 3; 4 ]) );
    ( "test remove, remove tail" >:: fun _ ->
      assert_equal (4, [ 1; 2; 3 ]) (remove 3 [ 1; 2; 3; 4 ]) );
  ]

let printer_tests =
  [
    ( "test string_of_tile on joker" >:: fun _ ->
      assert_equal "[JJ]" (Printer.string_of_tile joker) );
    ( "test string_of_tile on numbered tile 2Y" >:: fun _ ->
      assert_equal "[2Y]" (Printer.string_of_tile t1) );
    ( "test string_of_tile on numbered tile 13K" >:: fun _ ->
      assert_equal "[13K]" (Printer.string_of_tile t2) );
    ( "test string_of_row on short row" >:: fun _ ->
      assert_equal "[5R] [6R] [7R]" (Printer.string_of_row row1) );
    ( "test string_of_row on short row with joker" >:: fun _ ->
      assert_equal "[5K] [JJ] [7Y]" (Printer.string_of_row row2) );
  ]

let suite =
  "test suite for rummikaml"
  >::: List.flatten [ board_tests; game_tests; printer_tests ]

let () = run_test_tt_main suite
