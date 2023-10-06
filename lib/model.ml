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

  (** given three tiles of the same number, checks if their colors are different*)
  let check_same_num (t1 : tile) (t2 : tile) (t3 : tile) : bool = 
    match t1, t2, t3 with 
    | Num n1, Num n2, Num n3 -> n1.color != n2.color && n2.color != n3.color &&
      n1.color != n3.color
    | Joker, Num n1, Num n2 -> n1.color != n2.color 
    | Num n1, Joker, Num n2 -> n1.color != n2.color 
    | Num n1, Num n2, Joker -> n1.color != n2.color
    | _ -> true

  (** given three tiles of the same color, checks if they are in chronological order
  with a step of 1*)
  let check_same_color (t1 : tile) (t2 : tile) (t3 : tile) : bool =
    match t1, t2, t3 with
    | Num n1, Num n2, Num n3 -> if n2.num == n1.num + 1 && n3.num == n2.num + 1 
      then true else false
    | Joker, Num n1, Num n2 -> if n2.num == n1.num + 1 then true else false
    | Num n1, Joker, Num n2 -> if n2.num == n1.num + 2 then true else false
    | Num n1, Num n2, Joker -> if n2.num == n1.num + 1 then true else false
    | _ -> true

  (** given three tiles, checks if the that row of tiles is valid*)
  let valid_row (h1 : tile) (h2 : tile) (h3 : tile) : bool = 
    match h1, h2, h3 with 
    | Num n1, Num n2, Num n3 -> if n1.num == n2.num && n2.num == n3.num 
      then check_same_num h1 h2 h3 else 
      if n1.color == n2.color && n2.color == n3.color 
      then check_same_color h1 h2 h3 else false
    | Num n1, Num n2, Joker -> if n1.num == n2.num 
      then check_same_num h1 h2 h3 else 
      if n1.color == n2.color then check_same_color h1 h2 h3 else false
    | Num n1, Joker, Num n2 -> if n1.num == n2.num 
      then check_same_num h1 h2 h3 else 
      if n1.color == n2.color then check_same_color h1 h2 h3 else false
    | Joker, Num n1, Num n2 -> if n1.num == n2.num 
      then check_same_num h1 h2 h3 else 
      if n1.color == n2.color then check_same_color h1 h2 h3 else false 
    | _ -> true 
    
  let rec check (board : tile list list) : bool = 
    match board with 
    | [] -> true 
    | h :: t ->  
      match h with 
      | [] -> false
      | _ :: [] -> false
      | _ :: _ :: [] -> false
      | h1 :: h2 :: h3 :: [] -> valid_row h1 h2 h3
      | h1 :: h2 :: h3 :: h4 :: [] -> 
        (check_same_num h1 h2 h3 && check_same_num h2 h3 h4 && check_same_num h3 h4 h1) 
        || (valid_row h1 h2 h3 && valid_row h2 h3 h4)
      | h1 :: h2 :: h3 :: tt -> failwith "unimplemented"

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
  let make (names : string list) : game_state = failwith "unimplemented"
end
