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

(** A BoardType represents a Rummikaml board *)
module type BoardType = sig
  type t

  val add : t -> tile -> int * int -> t
  val move : t -> int * int -> int * int -> t
  val new_row : t -> tile -> t
  val check : t -> bool
  val check_first : t -> bool
end

(** A Board based on tiles *)
module Board : BoardType with type t = tile list list = struct
  (*every row is a list of tiles*)
  type t = tile list list

  let add (board : tile list list) (tile : tile) (loc : int * int) :
      tile list list =
    failwith "unimplemented"

  let move (board : tile list list) (startLoc : int * int) (endLoc : int * int)
      : tile list list =
    failwith "unimplemented"

  let new_row (board : tile list list) (tile : tile) : tile list list =
    failwith "unimplemented"

  let check (board : tile list list) : bool = failwith "unimplemented"
  let check_first (board : tile list list) : bool = failwith "unimplemented"
end

(** A GameType represents the whole state of a Rummikaml game *)
module type GameType = sig
  val next_player : player list -> player list
  val check_win : game_state -> player option
  val make : string list -> game_state
end

(** A Game represents the whole state of a Rummikaml game based on tiles *)
module Game = struct
  let next_player (players : player list) : player list =
    failwith "unimplemented"

  let check_win (state : game_state) : player option = failwith "unimplemented"
  let make = failwith "unimplemented"
end
