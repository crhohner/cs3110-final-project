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
    | Joker, Num n1, Num n2 | Num n1, Joker, Num n2 | Num n1, Num n2, Joker -> 
      n1.color != n2.color 
    | _ -> true

  (** given three tiles of the same color, checks if they are in chronological order
  with a step of 1*)
  let check_same_color (t1 : tile) (t2 : tile) (t3 : tile) : bool =
    match t1, t2, t3 with
    | Num n1, Num n2, Num n3 -> if n2.num == n1.num + 1 && n3.num == n2.num + 1 
      then true else false
    | Joker, Num n1, Num n2 | Num n1, Num n2, Joker -> if n2.num == n1.num + 1 
      then true else false
    | Num n1, Joker, Num n2 -> if n2.num == n1.num + 2 then true else false
    | _ -> true

  (** given three tiles, checks if the that row of tiles is valid*)
  let valid_row (h1 : tile) (h2 : tile) (h3 : tile) : bool = 
    match h1, h2, h3 with 
    | Num n1, Num n2, Num n3 -> if n1.num == n2.num && n2.num == n3.num 
      then check_same_num h1 h2 h3 else 
      if n1.color == n2.color && n2.color == n3.color 
      then check_same_color h1 h2 h3 else false
    | Num n1, Num n2, Joker | Num n1, Joker, Num n2 | Joker, Num n1, Num n2 -> 
      if n1.num == n2.num then check_same_num h1 h2 h3 else 
      if n1.color == n2.color then check_same_color h1 h2 h3 else false
    | _ -> true 
   
  (** given four tiles, checks if that row of tiles is valid*)
  let check_four (t1 : tile) (t2 : tile) (t3 : tile) (t4 : tile) : bool =
    match t1, t2, t3, t4 with
    | Num n1, Num n2, Num n3, Num n4 -> if n1.num == n2.num && n2.num == n3.num
      && n3.num == n4.num then (n1.color != n2.color && n2.color != n3.color 
      && n3.color != n4.color && n1.color != n3.color && n1.color != n4.color
      && n2.color != n4.color) else if n2.num == n1.num + 1 && n3.num == n2.num + 1
      && n4.num == n3.num + 1 then (n1.color == n2.color && n2.color == n3.color
      && n3.color == n4.color) else false
    | Joker, Num n1, Num n2, Num n3 -> valid_row t2 t3 t4
    | Num n1, Num n2, Num n3, Joker -> valid_row t1 t2 t4
    | Num n1, Num n2, Joker, Num n3 -> 
      if n1.num == n2.num && n2.num == n3.num then check_same_num t1 t2 t4 else 
      if n1.color == n2.color && n2.color == n3.color then 
      (n2.num == n1.num + 1 && n3.num == n2.num + 2) else false 
    | Num n1, Joker, Num n2, Num n3 -> 
      if n1.num == n2.num && n2.num == n3.num then check_same_num t1 t3 t4 else
      if n1.color == n2.color && n2.color == n3.color then
      (n2.num == n1.num + 2 && n3.num == n2.num + 1) else false 
    | Joker, Joker, Num n1, Num n2 | Joker, Num n1, Num n2, Joker | 
      Num n1, Num n2, Joker, Joker -> 
      if n1.num == n2.num then (n1.color != n2.color) else 
      if n1.color == n2.color then (n2.num == n1.num + 1) else false 
    | Joker, Num n1, Joker, Num n2 | Num n1, Joker, Num n2, Joker -> 
      if n1.num == n2.num then (n1.color != n2.color) else
      if n1.color == n2.color then (n2.num == n1.num + 2) else false 
    | Num n1, Joker, Joker, Num n2 -> 
      if n1.num == n2.num then (n1.color != n2.color) else 
      if n1.color == n2.color then (n2.num == n1.num + 3) else false 
    | _ -> false

  (** given a tile list and color, checks that all the tiles are same color*)
  let rec check_color (c : color) (tlst : tile list) : bool = 
    match tlst with 
    | [] -> true
    | h :: t -> 
      match h with 
      | Joker -> check_color c t
      | Num n -> n.color == c && check_color c t

  (** given a list of tiles, checks that they are in consecutive off-by-one order*)
  let rec check_row (num_joker : int) (tlst : tile list) : bool =
    if num_joker > 2 then false else 
    match tlst with 
    | [] -> true
    | _ :: [] -> true
    | h1 :: h2 :: [] -> 
      (match h1, h2 with
      | Num n1, Num n2 -> if n2.num == n1.num + 1 then true else false
      | Num n, Joker | Joker, Num n -> check_row (num_joker + 1) [] 
      | Joker, Joker -> check_row (num_joker + 2) [])
    | h1 :: h2 :: h3 :: [] -> 
      (match h1, h2, h3 with
      | Num n1, Num n2, Num n3 -> check_same_color h1 h2 h3
      | Num n1, Joker, Num n2 -> if num_joker + 1 > 2 then false else
        if n2.num == n1.num + 2 then true else false
      | Num n1, Num n2, Joker | Joker, Num n1, Num n2-> 
        if num_joker + 1 > 2 then false else
        if n2.num == n1.num + 1 then true else false
      | Joker, Joker, Num n | Joker, Num n, Joker | Num n, Joker, Joker -> 
        check_row (num_joker + 2) []
      | _ -> false)
    | h1 :: h2 :: h3 :: h4 :: t -> 
      match h1, h2, h3, h4 with
      | Joker, Joker, Joker, _ -> false
      | Joker, Joker, Num n, _ -> check_row (num_joker + 2) (h3 :: h4 :: t)
      | Joker, Num n1, Num n2, _ -> if n2.num == n1.num + 1 then 
        check_row (num_joker + 1) (h3 :: h4 :: t) else false 
      | Num n1, Joker, Num n2, _ -> if n2.num == n1.num + 2 then
        check_row (num_joker + 1) (h3 :: h4 :: t) else false 
      | Num n1, Num n2, Joker, Num n3 -> if n2.num == n1.num + 1 
        && n3.num == n2.num + 2 then check_row (num_joker + 1) (h4 :: t) 
        else false
      | Num n1, Num n2, Joker, Joker -> if n2.num == n1.num + 1 then 
        check_row (num_joker) (h3 :: h4 :: t) else false 
      | Num n1, Joker, Joker, Num n3 -> if n3.num == n1.num + 3 then 
        check_row (num_joker + 2) (h4 :: t) else false
      | Joker, Num n1, Joker, Num n2 -> if n2.num == n1.num + 2 then 
        check_row (num_joker + 2) (h4 :: t) else false 
      | Num n, Joker, Joker, Joker | Joker, Num n, Joker, Joker -> false
      | Num n1, Num n2, Num n3, _ -> if check_same_color h1 h2 h3 then
        check_row (num_joker) (h4 :: t) else false
      

  (** finds the color of the first non-Joker tile or returns false*)
  let get_color (t1 : tile) (t2 : tile) (t3 : tile) : bool * color =
    match t1 with 
    | Joker -> 
      (match t2 with 
      | Joker -> 
        (match t3 with 
        | Joker -> (false, Yellow)
        | Num n3 -> (true, n3.color))
      | Num n2 -> (true, n2.color)) 
    | Num n1 -> (true, n1.color)

  let rec check (board : tile list list) : bool = 
    match board with 
    | [] -> true 
    | h :: t ->  
      match h with 
      | [] -> false
      | _ :: [] -> false
      | _ :: _ :: [] -> false
      | h1 :: h2 :: h3 :: [] -> valid_row h1 h2 h3 && check t
      | h1 :: h2 :: h3 :: h4 :: [] -> check_four h1 h2 h3 h4 && check t
      | h1 :: h2 :: h3 :: tlist -> let c = get_color h1 h2 h3 in 
        match c with
        | (b, col) -> if b then let l = h1 :: h2 :: h3 :: tlist in 
          (if check_color col l then check_row 0 l && check t
          else false) else false

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
