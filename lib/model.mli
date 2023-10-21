type color =
  | Yellow
  | Red
  | Blue
  | Black

type tile =
  | Joker
  | Num of {
      num : int;
      color : color;
    }

type player = {
  hand : tile list;
  name : string;
}

type game_state = {
  players : player list;
  board : tile list list;
  deck : tile list;
}

val insert : 'a -> 'a list -> int -> 'a list
(** Returns [lst] with [ele] inserted at index [i]. Requires: 0 <= [i] <= length
    of [lst]. Raises Invalid_argument with invalid [i].*)

val replace : 'a -> 'a list -> int -> 'a list
(** Returns [lst] with [ele] in place of the [i]th element of [lst]. Requires: 0
    <= [i] < length of [lst]. Raises Invalid_argument with invalid [i].*)

val remove : int -> 'a list -> 'a * 'a list
(** Returns [lst] with the [i]th element removed, along with the removed element
    as an option. Requires: 0 <= [i] < length of [lst]. Raises Invalid_argument
    with invalid [i].*)

(** A BoardType represents a Rummikaml board *)
module type BoardType = sig
  (*Representation type*)
  type t = tile list list

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

  val check : t -> bool
  (**Returns whether a board configuration is valid, given a board [board]*)

  val check_first : t -> bool
  (**Returns whether a board configuration is valid, given a board [board]*)

  val valid_get_loc : tile list list -> int * int -> bool
  (**Returns whether a tile on [board] at [loc] exists: so whether loc is an
     existing position on the board.*)

  val valid_set_loc : tile list list -> int * int -> bool
  (**Returns whether a tile can be placed at [loc] on [board]. (length, 0) makes
     a new row. *)
end

module Board : BoardType

(** A GameType represents the whole state of a Rummikaml game *)
module type GameType = sig
  val next_player : game_state -> game_state
  (**Returns a game state [state] with the previous head/player moved to the end
     of the queue in [state.players] *)

  val check_win : player -> bool
  (**Returns whether given player has won the game, returns a bool *)

  val make : string list -> game_state
  (**Returns an initial game state before the first move, created from a list of
     player names*)

  val active_player : game_state -> player
  (**Returns the current player in a game [state]*)
end

module Game : GameType
