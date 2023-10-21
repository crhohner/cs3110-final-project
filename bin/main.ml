open Rummikaml
open Model
open Random
open View

(*input processing / repeated stuff goes here *)

(** Returns the first input from the command line that for which [p] is true.
    Will ask for a valid value by printing [msg] on inavlid input. *)
let rec get_input p msg =
  let input = read_line () in
  if p input then input
  else
    let _ = print_endline msg in
    get_input p msg

(** Returns the first valid integer input from the command line. Will ask for a
    valid integer if input is invalid. *)
let get_int () =
  int_of_string
    (get_input
       (fun x ->
         match int_of_string x with
         | exception Failure s -> false
         | _ -> true)
       "enter a valid integer input")

let tokenize (s : string) =
  let rec aux (str : string) (acc : string list) (curr : string) : string list =
    match String.get str 0 with
    | exception Invalid_argument s -> curr :: acc
    | ' ' -> aux (String.sub str 1 (String.length str - 1)) (curr :: acc) ""
    | c ->
        aux
          (String.sub str 1 (String.length str - 1))
          acc
          (curr ^ String.make 1 c)
  in

  List.rev (aux s [] "")

(** Returns a valid location of a desired tile in the board. Will restart entire 
    query process if either row or index are incorrect. *)
let rec get_tile_loc (board : tile list list) =
  print_endline ("enter row #: 0-" ^ string_of_int (List.length board - 1));
  let row_idx = get_int () in
  if row_idx < 0 || row_idx >= List.length board then
    let _ = print_endline "invalid row given, try again" in
    get_tile_loc board
  else
    let row = List.nth board row_idx in
    print_endline ("enter index #: 0-" ^ string_of_int (List.length row - 1));
    let tile_idx = get_int () in
    let loc = (row_idx, tile_idx) in
    if tile_idx >= 0 && tile_idx < List.length row then loc
    else
      let _ = print_endline "invalid index given, try again" in
      get_tile_loc board

(** Returns a valid location to place a new tile into the board. Will restart 
    entire query process if either row or index are incorrect. *)
let rec get_new_tile_loc (board : tile list list) =
  print_endline ("enter row #: 0-" ^ string_of_int (List.length board));
  let row_idx = get_int () in
  if row_idx < 0 || row_idx > List.length board then
    let _ = print_endline "invalid row given, try again" in
    get_new_tile_loc board
  else
    let row = List.nth board row_idx in
    print_endline ("enter index #: 0-" ^ string_of_int (List.length row));
    let tile_idx = get_int () in
    let loc = (row_idx, tile_idx) in
    if tile_idx >= 0 && tile_idx <= List.length row then loc
    else
      let _ = print_endline "invalid index given, try again" in
      get_new_tile_loc board

(** Returns a list of [n] player names given from the command line. Will
    substitute empty names with "player[n]" for the [n]th player. *)
let rec get_player_names n : string list =
  if n < 1 then []
  else
    let _ =
      print_endline ("player " ^ string_of_int n ^ ", what's your name?")
    in
    let name =
      match read_line () with
      | "" -> "player" ^ string_of_int n
      | name -> name
    in
    let _ = print_newline () in
    name :: get_player_names (n - 1)

(** Returns a valid location for the tile that a player wishes to select from their
    hand and add to the board. Will restart query process if index is incorrect. *)
let rec add_input (p : player) =
  let n = List.length p.hand - 1 in
  let _ =
    print_endline ("enter index of tile you want to add: 0-" ^ string_of_int n)
  in
  let idx = get_int () in
  if idx < 0 || idx > n then
    let _ =
      print_endline
        "this is an invalid index;\n  please enter a new, valid index: "
    in
    add_input p
  else idx

let rec turn (game : game_state) (start_state : game_state) (move : int) :
    game_state =
  (*add a move counter argument to track whether its the first move or not*)
  (*I think we will also need to track num of players so that we can tell first 
     move for each individual player as commented below*)
  let _ = CLIPrinter.show_turn game move in
  let player = Game.active_player game in
  let new_game, new_move =
    match read_line () with
    | "a" when move (*/numplayers*) > 0 ->
        (*extra draw branch added which is only available on the first move,
          move counter resets with invalid board at check_board *)
        let t_idx = add_input player in
        let tile, new_hand = Model.remove t_idx player.hand in
        let loc = get_new_tile_loc game.board in
        let new_board = Board.add game.board tile loc in
        let new_players =
          { player with hand = new_hand } :: List.tl game.players
        in
        ({ game with players = new_players; board = new_board }, move + 1)
    | "m" ->
        ( game,
          move + 1 (*create new game state with new board, updated player hand*)
        )
    | "e" -> (game, move + 1 (*this branch is already correct*))
    | "h" ->
        (game, move + 1 (*display help menu, then turn on the same game state*))
    | "r" -> (start_state, 0)
    | "d" when (move (*/numplayers*) = 0) && (List.length game.deck > 0)-> 
      let rand_tile = game.deck |> List.length |> Random.int in
      let tile, new_deck = Model.remove rand_tile game.deck in
      let new_players =
        { player with hand = tile::(player.hand) } :: List.tl game.players
      in
      ({ game with players = new_players; deck = new_deck }, move + 1)
    | _ -> (start_state, 0)
  in

  (*check board, win goes here before calling turn on the new game*)
  (*check board when turn is ended.. if fail reset move counter and call turn on
    first game state*)
  turn new_game start_state new_move

(*game starts here*)

let _ = print_newline ()

let _ =
  print_endline
    " [R]      _\n\
    \ [U]  .--' |\n\
    \ [M] /___^ |     .--.\n\
    \ [M]     ) |    /     \\\n\
    \ [I]    /  |  /`       '.\n\
    \ [K]   |   '-'    /      \\\n\
    \ [A]    \\         |       |\\\n\
    \ [M]     \\     /   \\      /\\|\n\
    \ [L]      \\  /'----`\\   /\n\
    \          |||        \\ |\n\
    \          ((|        ((|\n\
    \          |||        |||\n\
    \         //_(       //_("

let newlines = 20
let _ = print_endline "welcome to rummikaml! \n"
let _ = print_endline "let's get going. how many players?"
let player_count = get_int ()
let _ = print_newline ()
let player_names = get_player_names player_count
let game = Game.make player_names

module Printer = CLIPrinter

let _ =
  print_endline
    ((let active = Game.active_player game in
      active.name)
    ^ " goes first! let's play!")

let _ = print_newline ()

(*can uncomment below to see the first player's hand for funsies*)
let _ = CLIPrinter.show_turn game 0
