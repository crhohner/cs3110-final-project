type color (** variant representing color component of tile *)
type tile (** variant with Joker or Num of {num : int, color : color} *)
type player (** record with hand, tile list and name: str *)
type game_state (**game record*)

(* if you wanna make a new row in move, index is len of rows (1+max index)*)

(** Module that represents a rummikaml board. *)
module type Board = sig
  type 'a list 
  (** adding tile at certain location on board, tile to add, row to add it to, locATION to add it to in list??*)
  val add : tile -> int*int -> 'a list
  (**first tuple is row, index of og loc, second is row, index of new*)
  val move : int*int -> int*int -> 'a list
  (* adds a tile to a new row *)
  val new_row : tile -> int -> 'a list
  (** checking if board is valid *)
  val check : 'a list -> bool
  (*check if valid board after first turn, rows add up to at least 30*)
  val check_first: 'a list -> bool
end

(** Module that represents a rummikaml game. *)
module type Game = sig
  type game_state (**in a record, player list, deck (tile list) and a board*)
  (*moves head to back of list*)
  val next_player : player list -> player list
  (**returns winner or none*)
  val check_win : game_state -> player option
  val draw : game_state -> game_state
  (**starting state of the game, takes list of player names, max 2 to start*)
  val make : string list -> game_state

  
end
