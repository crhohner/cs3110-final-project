type color
type tile
type player
type game_state

(** A BoardType represents a Rummikaml board *)
module type BoardType = sig
  (*Representation type*)
  type t

  val add : t -> tile -> int * int -> t
  (**Returns a board with one tile added, given a board [board], a tile [tile],
     a location [loc] in the form (row, index)*)

  val move : t -> int * int -> int * int -> t
  (**Returns a board with one tile moved, given a board [board], the tile to be
     moved's current location [startLoc] in the form (row, index), and a
     location to be moved to [endLoc] in the form (row, index)*)

  val new_row : t -> tile -> t
  (**Returns a board [board] with a tile [tile] added as a new row beneath any
     existing rows*)

  val check : t -> int -> bool
  (**Returns whether a board configuration is valid, given a board [board]*)

  val check_first : t -> bool
  (**Returns whether a board configuration is valid, given a board [board]*)
end

module Board : BoardType

(** A GameType represents the whole state of a Rummikaml game *)
module type GameType = sig
  val next_player : player list -> player list
  (**Returns a player list [players] with the previous head/player moved to the
     end of the queue*)

  val check_win : game_state -> player option
  (**Returns whether a player has one the game, returns the winner or [None]*)

  val make : string list -> game_state
  (**Returns an initial game state, before the first moves, created from a list
     of player names*)
end

module Game : GameType
