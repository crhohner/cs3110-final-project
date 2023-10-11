open Rummikaml
open Model

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
    name :: get_player_names (n - 1)

let _ = print_endline "welcome to rummikaml!"
let _ = print_endline "let's get going. how many players?"
let player_count = get_int_input ()
let player_names = get_player_names player_count
let game = Game.make player_names
