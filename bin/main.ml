open Rummikaml
open Model
open Random

(*input processing / repeated stuff goes here *)
let rec get_int_input () : int =
  try read_int ()
  with Failure s ->
    let _ = print_endline "please enter a valid integer" in
    get_int_input ()

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

let rec set_first_player (names : string list) (name : string) : string list =
  match names with
  | h :: t -> if h = name then names else set_first_player (t @ [ h ]) name
  | [] -> failwith ("player " ^ name ^ "not found")

let choose_first_player (names : string list) =
  let _ = Random.self_init () in
  List.nth names (Random.int List.(length names))

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
    \ [M]     \\    /   \\      /\\|\n\
    \ [L]       \\  /'----`\\   /\n\
    \           |||        \\ |\n\
    \           ((|        ((|\n\
    \           |||        |||\n\
    \          //_(       //_("

let _ = print_endline "welcome to rummikaml! \n"
let _ = print_endline "let's get going. how many players?"
let _ = print_newline ()
let player_count = get_int_input ()
let player_names = get_player_names player_count
let first = choose_first_player player_names
let player_names = set_first_player player_names first
let game = Game.make player_names
let _ = print_endline (first ^ " goes first! let's play!")
