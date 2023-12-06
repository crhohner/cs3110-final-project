open Model

(** The signature of a CLI. *)
module type ViewType = sig
  val show_board : game_state -> unit
  (** Prints the board associated with a given game state [s]. *)

  val clear : int -> unit
  (** Takes in number [n]. Prints [n] new lines to wipe the last move from the
      CLI. *)

  val show_hand : player -> unit
  (** Prints a given player [p]'s current hand. *)

  val show_win : game_state -> unit
  (** Prints a message congratulating winning player [p] (the active player of
      the given game state [s]) at the end of the game. *)

  val show_turn : game_state -> bool -> unit
  (** Takes in game state [s] and boolean [altered]. Prints the board tied to
      [s], the current player's hand, and available actions based on whether the
      board has been altered ([altered]) in the active player's turn. *)

  val show_help : unit -> unit
  (** Prints the game's help menu. Player should need to press a key afterwards
      to clear the menu and display their turn. *)

  val string_of_tile : tile -> string
  (** Returns the string representation of a given tile [t] in the form
       "[<Num><Color>]". Examples: string_of_tile Joker = "[JJ]",
        string_of_tile Num {color = Yellow; num = 2} = "[2Y]". *)

  val string_of_row : tile list -> string
  (** Returns the string representation of a given row of tiles [r] in the form
      "[<Num><Color>] [<Num><Color>] ..." (spaced tile strings). Requires: [r]
      is non-empty. *)
end

module CLIPrinter : ViewType
(** A CLI. *)
