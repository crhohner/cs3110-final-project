(** Type representing the color of a tile. *)
type color =
  | Yellow
  | Red
  | Blue
  | Black

(** Type representing a tile - may be Joker or a tile with a number and color. *)
type tile =
  | Joker
  | Num of {
      num : int;
      color : color;
    }

type player = {
  hand : tile list;
  name : string;
  isCpu : bool;
}
(** Type representing a player - player assigned hand of tiles, name, and listed
    as CPU or not CPU. *)

type game_state = {
  players : player list;
  board : tile list list;
  deck : tile list;
}
(** Type representing the state of the game during a round - indicates players,
    the current state of the board, and the deck of tiles left (not in the hands
    of the players or on the board). *)

val insert : 'a -> 'a list -> int -> 'a list
(** Returns [lst] with given element [ele] inserted at index [i]. Requires: 0 <=
    [i] <= length of [lst]. Raises Invalid_argument with invalid [i].*)

val num_of_tile : tile -> int
(** Returns a numerical representation of a given tile [t]. Every tile is
    assigned an unique integer calculated based on their color and numbering
    (Joker is 0). *)

val replace : 'a -> 'a list -> int -> 'a list
(** Returns [lst] with given element [ele] in place of the [i]th element of
    [lst]. Requires: 0 <= [i] < length of [lst]. Raises Invalid_argument with
    invalid [i].*)

val remove : int -> 'a list -> 'a * 'a list
(** Returns [lst] with the [i]th element removed, along with the removed element
    as an option. Requires: 0 <= [i] < length of [lst]. Raises Invalid_argument
    with invalid [i].*)

val check_color : color -> tile list -> bool
(** Given a tile list [tlst] and color [c], checks that all the tiles are of the
    same color [c].*)

val check_num : int -> tile list -> bool
(** Given a tile list [tlst] and integer [n], checks that all the tiles are of
    the same number [n].*)

val check_rowcolors : tile list -> bool
(** Given list of tiles of the same number, checks if their colors are
    different. *)

val check_rownums : tile list -> bool
(** Given a list of tiles of the same color, checks that they are in consecutive
    off-by-one order. *)

val get_color : tile -> tile -> tile -> bool * color
(** Finds the color of the first non-Joker tile among the given three tiles
    [t1], [t2], and [t3]. Returns: [(false, Yellow)] if no color [c] is found,
    otherwise [(true, c)]. *)

val get_num : tile -> tile -> tile -> bool * int
(** Finds the number of the first non-Joker tile among the given three tiles
    [t1], [t2], and [t3]. Returns: [(false, 0)] if no number [n] is found,
    otherwise [(true, n)]. *)

val valid_row : color -> int -> tile list -> bool
(** Checks if a list of tiles is valid given a color and number. *)

(** The signature of a Rummikaml board. *)
module type BoardType = sig
  type t = tile list list
  (** Type representing a Rummikaml board (tile board). *)

  val add : t -> tile -> int * int -> t
  (** Takes in a board [b], a tile [tl], and a location [loc] in the form of
      [(row, index)]. Returns: The modified board with the tile [tl] added to
      [loc]. Requires: [loc] is a valid position on the board, else will throw
      an exception [Invalid_argument s]. *)

  val move : t -> int * int -> int * int -> t
  (** Takes in a board [b], the current location [startLoc] of the tile [t] you
      want to move on [b], and the new location [endLoc] of [t]. [startLoc] and
      [endLoc] are both provided in the form [(row, index)]. Returns: The
      modified board with [t] moved from [startLoc] to [endLoc]. Requires:
      [startLoc] and [endLoc] are valid positions on the board, else will throw
      an exception [Invalid_argument s]. *)

  val new_row : t -> tile -> t
  (** Takes in a board [b] and tile [t]. Returns: The modified board with [t]
      added as a new row beneath any existing rows of [b]. *)

  val check : t -> bool
  (** Returns whether a board configuration is valid, given a board [b]. *)

  val valid_get_loc : tile list list -> int * int -> bool
  (** Returns whether tile exists at given location [loc] of board [b]. *)

  val valid_set_loc : tile list list -> int * int -> bool
  (** Returns whether a tile can be placed at given location [loc] on board [b].
      (length, 0) produces a new row. *)
end

module Board : BoardType
(** A Rummikaml board. *)

(** The signature of the state of a Rummikaml game. *)
module type GameType = sig
  val next_player : game_state -> game_state
  (** Takes in a game state [s]. Returns: The modified game state with the
      previous player (head) moved to the end of the queue in [s.players]. *)

  val check_win : player -> bool
  (** Determines whether given player [p] has won the game. Returns: [true] if
      the hand of [p] is empty, [false] otherwise. *)

  val make : string list -> string list -> game_state
  (** Returns an initial game state before the first move, created from a given
      list of player names and list of CPU names. Distributes a random hand of
      14 tiles to every player/CPU from the deck. *)

  val active_player : game_state -> player
  (** Returns the current player in a given game state [s]. *)
end

module Game : GameType
(** A Rummikaml game. *)
