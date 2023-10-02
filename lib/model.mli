type color (** variant representing color component of tile *)
type tile (** variant with Joker or Num of {num : int, color : color} *)
type player (** record with hand, tile list and name: str *)

(** Module that represents a rummikaml board. *)
module type Board = sig
  type 'a list 
  (** adding tile at certain location on board *)
  val add : int -> int -> tile -> 'a list
  (* adds a tile to a new row *)
  val new_row : int -> tile -> 'a list
  (** checking if board is valid *)
  val check : 'a list -> bool
end

(** Module that represents a rummikaml game. *)
module type Game = sig
  val board : 'a list
  val players : player list
  
  
end
