open Model

(*implemented as CLI in model.ml, later maybe also GUI ?? >:) *)
module type ViewType = sig
  val show_board : game_state -> unit
  (**Prints the board associated with a given game state. *)

  val clear : int -> unit
  (**Returns prints [lines] newlines to wipe the last move from the CLI. *)

  val show_hand : player -> unit
  (**Returns Prints a player [player]'s current hand. *)

  val show_win : game_state -> player -> unit
  (**Returns Displays a win message at the end of the game. *)

  val show_turn : game_state -> unit
  (**Returns Displays the board, current player's hand, and available actions. *)

  val show_help : unit -> unit
  (**Returns Displays the game's help menu. Player should need to press a key
     afterwards to clear the menu and display their turn.*)

  val string_of_tile : tile -> string
  (**Returns the string representation of a tile [tile] in the form
       "[<Num><Color>]". Examples: string_of_tile Joker = "[JJ]",
        string_of_tile Num {color = Yellow; num = 2} = "[2Y]"*)
end

module CLIPrinter : ViewType
