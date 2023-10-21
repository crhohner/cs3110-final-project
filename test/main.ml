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
      Num { color = Black; num = 4 };
    ];
    [
      Num { color = Red; num = 10 };
      Num { color = Red; num = 11 };
      Num { color = Red; num = 12 };
    ];
  ]

let board10add1 =
  [
    [
      Num { color = Red; num = 4 };
      Num { color = Blue; num = 4 };
      Num { color = Yellow; num = 4 };
      Num { color = Black; num = 4 };
    ];
    [
      Num { color = Red; num = 10 };
      Num { color = Red; num = 11 };
      Num { color = Red; num = 12 };
    ];
  ]

let board10add2 =
  [
    [
      Num { color = Red; num = 4 };
      Num { color = Blue; num = 4 };
      Num { color = Black; num = 4 };
    ];
    [
      Num { color = Red; num = 10 };
      Num { color = Red; num = 11 };
      Num { color = Red; num = 12 };
      Num { color = Red; num = 13 };
    ];
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
    ];
  ]

let board13 =
  [
    [
      Num { color = Blue; num = 2 };
      Num { color = Yellow; num = 2 };
      Num { color = Red; num = 2 };
    ];
  ]

let board14 = [ [ Joker; Num { color = Red; num = 10 }; Joker ] ]

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
    ];
  ]

let board_a =
  [
    [
      Num { color = Yellow; num = 9 };
      Joker;
      Num { color = Yellow; num = 10 };
      Num { color = Yellow; num = 12 };
    ];
    [ Joker ];
  ]

let board_b =
  [
    [
      Num { color = Red; num = 10 };
      Num { color = Red; num = 11 };
      Num { color = Red; num = 13 };
    ];
    [ t2 ];
  ]

let board_c =
  [
    [
      Num { color = Red; num = 10 };
      Num { color = Red; num = 11 };
      Num { color = Red; num = 13 };
    ];
    [ t2 ];
    [ Joker ];
  ]

let board_d =
  [
    [
      Num { color = Red; num = 10 };
      Num { color = Red; num = 11 };
      Num { color = Red; num = 13 };
    ];
    [ t2; Joker ];
  ]

let board_e =
  [
    [
      Num { color = Red; num = 11 };
      Num { color = Red; num = 13 };
      Num { color = Red; num = 10 };
    ];
    [ t2 ];
  ]

let board_f =
  [
    [ Num { color = Red; num = 11 }; Num { color = Red; num = 13 } ];
    [ t2; Num { color = Red; num = 10 } ];
  ]

let board_g =
  [
    [
      Num { color = Red; num = 11 };
      Num { color = Red; num = 13 };
      t2;
      Num { color = Red; num = 10 };
    ];
    [];
  ]

let board_h =
  [
    [
      Num { color = Red; num = 11 };
      Num { color = Red; num = 13 };
      t2;
      Num { color = Red; num = 10 };
    ];
  ]

module Printer = CLIPrinter

let board_tests =
  [
    (*add tests*)
    ( "tile at front of one row board" >:: fun _ ->
      assert_equal board8add
        (Board.add board8 (Num { color = Yellow; num = 9 }) (0, 0)) );
    ( "tile in first row" >:: fun _ ->
      assert_equal board10add1
        (Board.add board10 (Num { color = Yellow; num = 4 }) (0, 2)) );
    ( "tile at end of last row" >:: fun _ ->
      assert_equal board10add2
        (Board.add board10 (Num { color = Red; num = 13 }) (1, 3)) );
    (*move tests*)
    ( "move tile to same pos" >:: fun _ ->
      assert_equal board_a (Board.move board_a (0, 0) (0, 0)) );
    ( "move tile to same pos" >:: fun _ ->
      assert_equal board_d (Board.move board_d (1, 1) (1, 1)) );
    ( "move tile across same row" >:: fun _ ->
      assert_equal board_e (Board.move board_b (0, 0) (0, 2)) );
    ( "move tile across diff row" >:: fun _ ->
      assert_equal board_f (Board.move board_e (0, 2) (1, 1)) );
    ( "move last tile of its row" >:: fun _ ->
      assert_equal board_g (Board.move board_e (1, 0) (0, 2)) );
    (* this is an interesting case – would it be better to remove the entire row
       in this instance? *)
    ( "move last tile of its row" >:: fun _ ->
      assert_equal board_g (Board.move board_e (1, 0) (0, 2)) );
    (* another interesting case below, commented out; not sure on exact
       functionality but I would assume move can move a tile to from one row to
       a new one *)
    (* "move tile to new row" >:: fun _ -> assert_equal board_e (Board.move
       board_h (0,2) (1,0)) *)
    (*new_row tests*)
    ( "adding tile to empty row" >:: fun _ ->
      assert_equal [ [ Joker ] ] (Board.new_row [] Joker) );
    ( "adding joker to new row" >:: fun _ ->
      assert_equal board_a (Board.new_row board17 Joker) );
    ( "adding num tile to new row" >:: fun _ ->
      assert_equal board_b (Board.new_row board15 t2) );
    ( "adding num tile to board with >1 rows" >:: fun _ ->
      assert_equal board_c (Board.new_row board_b Joker) );
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
    ("one set two jokers" >:: fun _ -> assert_equal true (Board.check board6));
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

let player_1 =
  { hand = [ Joker; Num { num = 1; color = Blue } ]; name = "Alice" }

let player_2 =
  {
    hand = [ Num { num = 2; color = Yellow }; Num { num = 10; color = Blue } ];
    name = "Grace";
  }

let player_3 =
  { hand = [ Num { num = 8; color = Red }; Joker ]; name = "David" }

let player_4 =
  {
    hand = [ Num { num = 9; color = Red }; Num { num = 2; color = Yellow } ];
    name = "Joy";
  }

let gs_no_players = { players = []; board = board1; deck = [] }
let gs_single_player = { players = [ player_1 ]; board = board1; deck = [] }

let gs_two_players =
  { players = [ player_2; player_4 ]; board = board1; deck = [] }

let gs_multi_player =
  {
    players = [ player_1; player_2; player_3; player_4 ];
    board = board1;
    deck = [];
  }

let make_single_player = Game.make [ "Alice" ]
let make_two_players = Game.make [ "Alice"; "Grace" ]
let make_multi_player = Game.make [ "Alice"; "Grace"; "David"; "Joy" ]

(* -------------------- Helpers to test make -------------------- *)
(* Helper that maps given tile to an integer *)
let num_of_tile (t : tile) : int =
  match t with
  | Joker -> 0
  | Num { num = n; color = c } -> (
      match c with
      | Yellow -> 100 + n
      | Red -> 200 + n
      | Blue -> 300 + n
      | Black -> 400 + n)

(* Helper that compares two lists and determines whether they contain the same
   elements - order doesn't matter *)
let cmp (lst1 : tile list) (lst2 : tile list) : bool =
  let sort1 = List.sort (fun t1 t2 -> num_of_tile t1 - num_of_tile t2) lst1 in
  let sort2 = List.sort (fun t1 t2 -> num_of_tile t1 - num_of_tile t2) lst2 in
  sort1 = sort2

(* Helper that takes in list of players and returns all tiles in players'
   hands*)
let all_hands (lst : player list) : tile list =
  List.fold_left (fun acc player -> acc @ player.hand) [] lst

(* Helper used to create a complete deck of tiles *)
let rec create_deck (n : int) (lst : tile list) : tile list =
  if n = 0 then [ Joker; Joker ]
  else
    let l =
      [
        Num { num = n; color = Yellow };
        Num { num = n; color = Red };
        Num { num = n; color = Blue };
        Num { num = n; color = Black };
      ]
    in
    create_deck (n - 1) lst @ l @ l

(* -------------------- End of helpers to test make -------------------- *)

let entire_deck = create_deck 13 []

let tiles1 =
  [
    Num { color = Blue; num = 1 };
    Num { color = Red; num = 2 };
    Num { color = Black; num = 1 };
  ]

let tiles2 = [ Num { color = Red; num = 10 } ]

let tiles3 =
  [
    Num { color = Red; num = 10 };
    Num { color = Red; num = 11 };
    Joker;
    Num { color = Red; num = 13 };
  ]

let winner1 = { hand = []; name = "manolis ;)" }
let winner2 = { hand = []; name = "still manolis ;)" }
let loser1 = { hand = tiles1; name = "loser1" }
let loser2 = { hand = tiles2; name = "loser2" }
let loser3 = { hand = tiles3; name = "loser3" }

let game_tests =
  [
    (* unit tests of next_player *)
    ( "test next_player when no players" >:: fun _ ->
      assert_equal [] (Game.next_player gs_no_players).players );
    ( "test next_player when single player" >:: fun _ ->
      assert_equal [ player_1 ] (Game.next_player gs_single_player).players );
    ( "test next_player when two players" >:: fun _ ->
      assert_equal [ player_4; player_2 ]
        (Game.next_player gs_two_players).players );
    ( "test next_player when multiple players" >:: fun _ ->
      assert_equal
        [ player_2; player_3; player_4; player_1 ]
        (Game.next_player gs_multi_player).players );
    ( "test next_player many times when single player" >:: fun _ ->
      assert_equal [ player_1 ]
        (gs_single_player |> Game.next_player |> Game.next_player).players );
    ( "test next_player many times when multiple players" >:: fun _ ->
      assert_equal
        [ player_4; player_1; player_2; player_3 ]
        (gs_multi_player |> Game.next_player |> Game.next_player
       |> Game.next_player)
          .players );
    (* unit tests of make *)
    (let p = make_single_player.players in
     let h = (List.hd p).hand in
     "test hand size, single player" >:: fun _ ->
     assert_equal 14 (List.length h));
    (let d = make_single_player.deck in
     "test deck size, single player" >:: fun _ ->
     assert_equal 92 (List.length d));
    (let p = make_single_player.players in
     let h = all_hands p in
     let full = h @ make_single_player.deck in
     "test entire deck, single player" >:: fun _ ->
     assert_equal true (cmp full entire_deck));
    (let p = make_two_players.players in
     let h1 = (List.nth p 0).hand in
     let h2 = (List.nth p 1).hand in
     "test hand size, two players" >:: fun _ ->
     assert_equal true (List.length h1 = 14 && List.length h2 = 14));
    (let d = make_two_players.deck in
     "test deck size, two players" >:: fun _ -> assert_equal 78 (List.length d));
    (let p = make_two_players.players in
     let h = all_hands p in
     let full = h @ make_two_players.deck in
     "test entire deck, two players" >:: fun _ ->
     assert_equal true (cmp full entire_deck));
    (let p = make_multi_player.players in
     let h1 = (List.nth p 0).hand in
     let h2 = (List.nth p 1).hand in
     let h3 = (List.nth p 2).hand in
     let h4 = (List.nth p 3).hand in
     "test hand size, multiple players" >:: fun _ ->
     assert_equal true
       (List.length h1 = 14
       && List.length h2 = 14
       && List.length h3 = 14
       && List.length h4 = 14));
    (let d = make_multi_player.deck in
     "test deck size, multiple players" >:: fun _ ->
     assert_equal 50 (List.length d));
    (let p = make_multi_player.players in
     let h = all_hands p in
     let full = h @ make_multi_player.deck in
     "test entire deck, multiple players" >:: fun _ ->
     assert_equal true (cmp full entire_deck));
    (* unit tests of next_player using make *)
    (*check_win tests*)
    (** (let game = { players = []; board = []; deck = [] } in
     "returns None on empty player list" >:: fun _ ->
     assert_equal None (Game.check_win game));
    (let game = { players = [ winner1 ]; board = []; deck = [] } in
     "returns winning player on single player list" >:: fun _ ->
     assert_equal (Some winner1) (Game.check_win game));
    (let loser = { hand = [ Joker ]; name = "loser" } in
     let game = { players = [ loser ]; board = []; deck = [] } in
     "returns None on single player list no winners" >:: fun _ ->
     assert_equal None (Game.check_win game));
    (let game =
       { players = [ loser1; loser2; loser3 ]; board = []; deck = [] }
     in
     "returns None on >1 player list no winners" >:: fun _ ->
     assert_equal None (Game.check_win game));
    (let game =
       { players = [ winner1; loser1; loser2; loser3 ]; board = []; deck = [] }
     in
     "returns winner on >1 player list 1 winner at front" >:: fun _ ->
     assert_equal (Some winner1) (Game.check_win game));
    (let game =
       { players = [ loser1; loser2; winner1; loser3 ]; board = []; deck = [] }
     in
     "returns winner on >1 player list 1 winner not at front" >:: fun _ ->
     assert_equal (Some winner1) (Game.check_win game));
    (*special test case: this may not happen depending on code implementation*)
    (let game =
       {
         players = [ loser1; winner2; loser2; winner1; loser3 ];
         board = [];
         deck = [];
       }
     in
     "returns frontmost winner on >1 player list >1 winner" >:: fun _ ->
     assert_equal (Some winner2) (Game.check_win game)); *)
    (*active_player tests*)
    (let game = { players = [ winner1 ]; board = []; deck = [] } in
     "single player list returns only player" >:: fun _ ->
     assert_equal winner1 (Game.active_player game));
    (let game =
       { players = [ winner1; winner2; loser1 ]; board = []; deck = [] }
     in
     "multi player list returns first player" >:: fun _ ->
     assert_equal winner1 (Game.active_player game));
    (let rec all_active state =
       match state.players with
       | [] -> []
       | h :: t ->
           Game.active_player
             { players = h :: t; board = state.board; deck = state.deck }
           :: all_active { players = t; board = state.board; deck = state.deck }
     in
     let all_players = [ winner1; winner2; loser1; loser2; loser3 ] in
     let game = { players = all_players; board = []; deck = [] } in
     "running active_player on all players" >:: fun _ ->
     assert_equal all_players (all_active game));
  ]

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
