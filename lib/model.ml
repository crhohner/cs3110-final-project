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
  val check : t -> int -> bool
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
    match (t1, t2, t3) with
    | Num n1, Num n2, Num n3 ->
        n1.color != n2.color && n2.color != n3.color && n1.color != n3.color
    | Joker, Num n1, Num n2 | Num n1, Joker, Num n2 | Num n1, Num n2, Joker ->
        n1.color != n2.color
    | _ -> true

  (** given three tiles of the same color, checks if they are in chronological
      order with a step of 1*)
  let check_same_color (t1 : tile) (t2 : tile) (t3 : tile) : bool =
    match (t1, t2, t3) with
    | Num n1, Num n2, Num n3 ->
        if n2.num == n1.num + 1 && n3.num == n2.num + 1 then true else false
    | Joker, Num n1, Num n2 | Num n1, Num n2, Joker ->
        if n2.num == n1.num + 1 then true else false
    | Num n1, Joker, Num n2 -> if n2.num == n1.num + 2 then true else false
    | _ -> true

  (** given three tiles, checks if the that row of tiles is valid*)
  let valid_row (h1 : tile) (h2 : tile) (h3 : tile) (joker : int) : bool * int =
    match (h1, h2, h3) with
    | Num n1, Num n2, Num n3 ->
        if n1.num == n2.num && n2.num == n3.num then
          (check_same_num h1 h2 h3, joker)
        else if n1.color == n2.color && n2.color == n3.color then
          (check_same_color h1 h2 h3, joker)
        else (false, joker)
    | Num n1, Num n2, Joker | Num n1, Joker, Num n2 | Joker, Num n1, Num n2 ->
        if n1.num == n2.num then (check_same_num h1 h2 h3, joker + 1)
        else if n1.color == n2.color then (check_same_color h1 h2 h3, joker + 1)
        else (false, joker + 1)
    | Num _, Joker, Joker | Joker, Joker, Num _ -> (true, joker + 2)
    | _ -> (false, joker + 3)

  (** given four tiles, checks if that row of tiles is valid*)
  let check_four (t1 : tile) (t2 : tile) (t3 : tile) (t4 : tile) (joker : int) :
      bool * int =
    match (t1, t2, t3, t4) with
    | Num n1, Num n2, Num n3, Num n4 ->
        if n1.num == n2.num && n2.num == n3.num && n3.num == n4.num then
          ( n1.color != n2.color && n2.color != n3.color && n3.color != n4.color
            && n1.color != n3.color && n1.color != n4.color
            && n2.color != n4.color,
            joker )
        else if
          n2.num == n1.num + 1 && n3.num == n2.num + 1 && n4.num == n3.num + 1
        then
          ( n1.color == n2.color && n2.color == n3.color && n3.color == n4.color,
            joker )
        else (false, joker)
    | Joker, Num n1, Num n2, Num n3 -> valid_row t2 t3 t3 (joker + 1)
    | Num n1, Num n2, Num n3, Joker -> valid_row t1 t2 t4 (joker + 1)
    | Num n1, Num n2, Joker, Num n3 ->
        if n1.num == n2.num && n2.num == n3.num then
          (check_same_num t1 t2 t4, joker + 1)
        else if n1.color == n2.color && n2.color == n3.color then
          (n2.num == n1.num + 1 && n3.num == n2.num + 2, joker + 1)
        else (false, joker + 1)
    | Num n1, Joker, Num n2, Num n3 ->
        if n1.num == n2.num && n2.num == n3.num then
          (check_same_num t1 t3 t4, joker + 1)
        else if n1.color == n2.color && n2.color == n3.color then
          (n2.num == n1.num + 2 && n3.num == n2.num + 1, joker + 1)
        else (false, joker + 1)
    | Joker, Joker, Num n1, Num n2
    | Joker, Num n1, Num n2, Joker
    | Num n1, Num n2, Joker, Joker ->
        if n1.num == n2.num then (n1.color != n2.color, joker + 2)
        else if n1.color == n2.color then (n2.num == n1.num + 1, joker + 2)
        else (false, joker + 2)
    | Joker, Num n1, Joker, Num n2 | Num n1, Joker, Num n2, Joker ->
        if n1.num == n2.num then (n1.color != n2.color, joker + 2)
        else if n1.color == n2.color then (n2.num == n1.num + 2, joker + 2)
        else (false, joker + 2)
    | Num n1, Joker, Joker, Num n2 ->
        if n1.num == n2.num then (n1.color != n2.color, joker + 2)
        else if n1.color == n2.color then (n2.num == n1.num + 3, joker + 2)
        else (false, joker + 2)
    | _ -> (false, joker)

  (** given a tile list and color, checks that all the tiles are same color*)
  let rec check_color (c : color) (tlst : tile list) : bool =
    match tlst with
    | [] -> true
    | h :: t -> (
        match h with
        | Joker -> check_color c t
        | Num n -> n.color == c && check_color c t)

  (** given a list of tiles, checks that they are in consecutive off-by-one
      order*)
  let rec check_row (num_joker : int) (tlst : tile list) : bool * int =
    if num_joker > 2 then (false, num_joker)
    else
      match tlst with
      | [] -> (true, num_joker)
      | _ :: [] -> (true, num_joker)
      | [ h1; h2 ] -> (
          match (h1, h2) with
          | Num n1, Num n2 ->
              if n2.num == n1.num + 1 then (true, num_joker)
              else (false, num_joker)
          | Num n, Joker | Joker, Num n -> check_row (num_joker + 1) []
          | Joker, Joker -> check_row (num_joker + 2) [])
      | [ h1; h2; h3 ] -> (
          match (h1, h2, h3) with
          | Num n1, Num n2, Num n3 -> (check_same_color h1 h2 h3, num_joker)
          | Num n1, Joker, Num n2 ->
              if num_joker + 1 > 2 then (false, num_joker)
              else if n2.num == n1.num + 2 then (true, num_joker)
              else (false, num_joker)
          | Num n1, Num n2, Joker | Joker, Num n1, Num n2 ->
              if num_joker + 1 > 2 then (false, num_joker)
              else if n2.num == n1.num + 1 then (true, num_joker)
              else (false, num_joker)
          | Joker, Joker, Num n | Joker, Num n, Joker | Num n, Joker, Joker ->
              check_row (num_joker + 2) []
          | _ -> (false, num_joker))
      | h1 :: h2 :: h3 :: h4 :: t -> (
          match (h1, h2, h3, h4) with
          | Joker, Joker, Joker, _ -> (false, num_joker)
          | Joker, Joker, Num n, _ -> check_row (num_joker + 2) (h3 :: h4 :: t)
          | Joker, Num n1, Num n2, _ ->
              if n2.num == n1.num + 1 then
                check_row (num_joker + 1) (h3 :: h4 :: t)
              else (false, num_joker)
          | Num n1, Joker, Num n2, _ ->
              if n2.num == n1.num + 2 then
                check_row (num_joker + 1) (h3 :: h4 :: t)
              else (false, num_joker)
          | Num n1, Num n2, Joker, Num n3 ->
              if n2.num == n1.num + 1 && n3.num == n2.num + 2 then
                check_row (num_joker + 1) (h4 :: t)
              else (false, num_joker)
          | Num n1, Num n2, Joker, Joker ->
              if n2.num == n1.num + 1 then check_row num_joker (h3 :: h4 :: t)
              else (false, num_joker)
          | Num n1, Joker, Joker, Num n3 ->
              if n3.num == n1.num + 3 then check_row (num_joker + 2) (h4 :: t)
              else (false, num_joker)
          | Joker, Num n1, Joker, Num n2 ->
              if n2.num == n1.num + 2 then check_row (num_joker + 2) (h4 :: t)
              else (false, num_joker)
          | Num n, Joker, Joker, Joker | Joker, Num n, Joker, Joker ->
              (false, num_joker)
          | Num n1, Num n2, Num n3, _ ->
              if check_same_color h1 h2 h3 then check_row num_joker (h4 :: t)
              else (false, num_joker))

  (** finds the color of the first non-Joker tile or returns false*)
  let get_color (t1 : tile) (t2 : tile) (t3 : tile) : bool * color =
    match t1 with
    | Joker -> (
        match t2 with
        | Joker -> (
            match t3 with
            | Joker -> (false, Yellow)
            | Num n3 -> (true, n3.color))
        | Num n2 -> (true, n2.color))
    | Num n1 -> (true, n1.color)

  let rec check (board : tile list list) (jokers : int) : bool =
    if jokers > 2 then false
    else
      match board with
      | [] -> true
      | h :: t -> (
          match h with
          | [] -> true
          | _ :: [] -> false
          | [ _; _ ] -> false
          | [ h1; h2; h3 ] -> (
              let checked = valid_row h1 h2 h3 jokers in
              match checked with
              | b, joker -> if b then check t joker else false)
          | [ h1; h2; h3; h4 ] -> (
              let checked = check_four h1 h2 h3 h4 jokers in
              match checked with
              | b, joker -> if b then check t joker else false)
          | h1 :: h2 :: h3 :: tlist -> (
              let c = get_color h1 h2 h3 in
              match c with
              | b, col ->
                  if b then
                    let l = h1 :: h2 :: h3 :: tlist in
                    if check_color col l then
                      let checked = check_row jokers l in
                      match checked with
                      | b, joker -> if b then check t joker else false
                    else false
                  else false))

  let check_first (board : tile list list) : bool = failwith "unimplemented"
end

(** A GameType represents the whole state of a Rummikaml game *)
module type GameType = sig
  val next_player : game_state -> game_state
  val check_win : game_state -> player option
  val make : string list -> game_state
  val active_player : game_state -> player
end

(** A Game represents the whole state of a Rummikaml game based on tiles *)
module Game : GameType = struct
  let next_player (state : game_state) : game_state =
    let next_lst =
      match state.players with
      | h :: t -> t @ [ h ]
      | _ -> []
    in
    { state with players = next_lst }

  (* Function checks if a player has won (no more tiles) - currently iterates
     through entire list of players rather than simply checking the current
     player. Depends on how often we run check_win *)
  let rec check_win (state : game_state) : player option =
    let rec empty_hand (lst : player list) : player option =
      match lst with
      | [] -> None
      | h :: t -> if h.hand = [] then Some h else empty_hand t
    in
    empty_hand state.players

  (* Helper for make function used to create a complete deck of tiles *)
  let rec create_deck (n : int) (lst : tile list) : tile list =
    if n = 0 then [ Joker; Joker ]
    else
      let l =
        [
          Num { num = n; color = Yellow };
          Num { num = n; color = Red };
          Num { num = n; color = Blue };
          Num { num = n; color = Black };
        ]
      in
      create_deck (n - 1) lst @ l @ l

  (** Helper for subset function that removes nth element in given list -
      returns tuple of element removed and modified list *)
  let rec pop (n : int) (lst : tile list) : tile * tile list =
    if n = 0 then (List.hd lst, List.tl lst)
    else
      let r = pop (n - 1) (List.tl lst) in
      (fst r, List.hd lst :: snd r)

  (** Helper for distribute_hand function that randomly takes n elements from
      given list - returns tuple of n random elements and modified list *)
  let rec subset (n : int) (lst : tile list) : tile list * tile list =
    if n = 0 then ([], lst)
    else
      let _ = Random.self_init () in
      let popped = pop (Random.int (List.length lst)) lst in
      let lsts = subset (n - 1) (snd popped) in
      (fst popped :: fst lsts, snd lsts)

  (** Helper for make function that distributes 14 random starting tiles from
      the deck to each player - returns tuple of player list with hands and
      remaining deck *)
  let rec distribute_hand (players : player list) (deck : tile list) :
      player list * tile list =
    if players = [] then ([], deck)
    else
      let s = subset 14 deck in
      let player = List.hd players in
      let remaining = List.tl players in
      ( { player with hand = fst s } :: fst (distribute_hand remaining (snd s)),
        snd (distribute_hand remaining (snd s)) )

  (** Returns a game state [state] with a randomly selected player as the head
      of the player queue, [state.players]*)
  let choose_first_player (state : game_state) =
    let _ = Random.self_init () in
    let idx = Random.int (List.length state.players) in
    let rec set_first (n : int) (players : player list) : player list =
      match players with
      | h :: t -> if n = 0 then players else set_first (n - 1) (t @ [ h ])
      | [] -> raise (Failure "player not found")
    in
    { state with players = set_first idx state.players }

  (** Function that takes in names of players and creates initial game_state.
      Note board is empty. *)
  let make (names : string list) : game_state =
    let p = List.map (fun n -> { hand = []; name = n }) names in
    let dis = distribute_hand p (create_deck 13 []) in
    choose_first_player { players = fst dis; board = []; deck = snd dis }

  let active_player (state : game_state) = List.hd state.players
end
