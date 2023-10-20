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

(*let tile_of_string (str : string) : tile =
  let n = String.make 1 (str.[0]) in
  let c = String.make 1 (str.[1]) in
  match str with 
  | “JJ” -> Joker
  | _ -> if ()
    match c with
      | “Y” -> Num {num = int_of_string n; color = Yellow}
      | “R” -> Num {num = int_of_string n; color = Red}
      | “K” -> Num {num = int_of_string n; color = Black}
      | “B” -> Num {num = int_of_string n; color = Blue}*)


let a_input (b : tile list list) : tile list list = 
  let _ = print_endline ("what tile do you want to add?") in
  let t = read_line () in
  let _ = print_endline ("which group of tiles do you want to add to? 
          (enter row number corresponding with group)") in 
  let row = int_of_string (read_line ()) in
  let _ = print_endline ("where in the group of tile do you want to add to? 
          (enter number corresponding with position)") in
  let idx = int_of_string (read_line ()) in
  (Board.add b t (row - 1, idx - 1))
  
let n_input (b : tile list list) : bool = 
  let _ = print_endline ("what tile do you want to add?") in false


let rec turn (b: tile list list) : bool = 
  let s = read_line () in
  match s with
  | "a" -> let _ = print_endline ("you have chosen to add a tile") in 
    let board = a_input b in 
    let _ = CLIPrinter.show_turn in turn board
  | "n" -> let _ = print_endline ("you have chosen to add a new row") in false
  | "m" -> let _ = print_endline ("you have chosen to move a tile") in false
  | "e" -> let _ = print_endline ("you have chosen to end your turn") in 
    Board.check b
  | "h" -> let _ = print_endline ("you have chosen to get help") in false 
  |  _ -> let _ = print_endline ("invalid input. please enter a valid input") in 
    turn b

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
let _ = CLIPrinter.show_turn game
