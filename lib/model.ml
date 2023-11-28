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
  isCpu : bool;
}

type game_state = {
  players : player list;
  board : tile list list;
  deck : tile list;
}

let num_of_tile (t : tile) : int =
  match t with
  | Joker -> 0
  | Num { num = n; color = c } -> (
      match c with
      | Yellow -> 100 + n
      | Red -> 200 + n
      | Blue -> 300 + n
      | Black -> 400 + n)

let rec insert (ele : 'a) (lst : 'a list) (i : int) : 'a list =
  if i > List.length lst || i < 0 then raise (Invalid_argument "invalid loc")
  else
    match lst with
    | [] -> [ ele ]
    | h :: t when i > 0 -> h :: insert ele t (i - 1)
    | _ -> ele :: lst

let rec replace (ele : 'a) (lst : 'a list) (i : int) : 'a list =
  match lst with
  | [] -> raise (Invalid_argument "invalid index")
  | h :: t when i > 0 -> h :: replace ele t (i - 1)
  | _ :: t -> ele :: t

let rec remove (i : int) (lst : 'a list) : 'a * 'a list =
  match lst with
  | [] -> raise (Invalid_argument "invalid index")
  | h :: t when i > 0 ->
      let next = remove (i - 1) t in
      (fst next, h :: snd next)
  | h :: t -> (h, t)

(** A BoardType represents a Rummikaml board *)
module type BoardType = sig
  type t = tile list list

  val add : t -> tile -> int * int -> t
  val move : t -> int * int -> int * int -> t
  val new_row : t -> tile -> t
  val check : t -> bool
  val check_first : t -> bool
  val valid_get_loc : tile list list -> int * int -> bool
  val valid_set_loc : tile list list -> int * int -> bool
end

(** A Board based on tiles *)
module Board : BoardType with type t = tile list list = struct
  (*every row is a list of tiles*)
  type t = tile list list

  (*Add will throw an exception (Failure s) if you try to add to an invalid
    position.*)
  let add (board : tile list list) (tile : tile) (loc : int * int) :
      tile list list =
    if loc = (List.length board, 0) then board @ [ [ tile ] ]
    else
      let row = List.nth board (fst loc) in
      let new_row = insert tile row (snd loc) in
      replace new_row board (fst loc)

  (*Move will throw an exception (Failure s) if you try to add to an invalid
    position or take a tile from an invalid position.*)
  let move (board : tile list list) (startLoc : int * int) (endLoc : int * int)
      : tile list list =
    let row = List.nth board (fst startLoc) in
    let tile, new_row = remove (snd startLoc) row in
    let new_end =
      if fst startLoc = fst endLoc && snd endLoc = List.length row then
        snd endLoc - 1
      else snd endLoc
    in
    let mid_board = replace new_row board (fst startLoc) in
    add mid_board tile (fst endLoc, new_end)

  let valid_get_loc (board : tile list list) (loc : int * int) : bool =
    if fst loc < 0 || snd loc < 0 then false
    else if fst loc >= List.length board then false
    else
      let row = List.nth board (fst loc) in
      snd loc < List.length row

  let valid_set_loc (board : tile list list) (loc : int * int) : bool =
    if fst loc < 0 || snd loc < 0 then false
    else if fst loc > List.length board then false
    else if fst loc = List.length board then snd loc = 0
    else
      let row = List.nth board (fst loc) in
      snd loc <= List.length row

  (** given a new tile, returns a board with the tile placed in a new row *)
  let new_row (board : tile list list) (tile : tile) : tile list list =
    board @ [ [ tile ] ]

  (** given a tile list and color, checks that all the tiles are same color*)
  let rec check_color (c : color) (tlst : tile list) : bool =
    match tlst with
    | [] -> true
    | h :: t -> (
        match h with
        | Joker -> check_color c t
        | Num n -> n.color == c && check_color c t)

  (** given a tile list and integer, checks that all the tiles are same number*)
  let rec check_num (n : int) (tlst : tile list) : bool =
    match tlst with
    | [] -> true
    | h :: t -> (
        match h with
        | Joker -> check_num n t
        | Num n1 -> n1.num == n && check_num n t)

  (** given list of tiles of the same number, checks if their colors are
      different*)
  let check_rowcolors (tlst : tile list) : bool =
    match tlst with
    | [ t1; t2; t3 ] -> (
        match (t1, t2, t3) with
        | Num n1, Num n2, Num n3 ->
            n1.color != n2.color && n2.color != n3.color && n1.color != n3.color
        | Joker, Num n1, Num n2 | Num n1, Joker, Num n2 | Num n1, Num n2, Joker
          -> n1.color != n2.color
        | _ -> true)
    | [ t1; t2; t3; t4 ] -> (
        match (t1, t2, t3, t4) with
        | Num n1, Num n2, Num n3, Num n4 ->
            n1.color != n2.color && n2.color != n3.color && n3.color != n4.color
            && n4.color != n1.color && n3.color != n1.color
            && n4.color != n2.color
        | Num n1, Num n2, Num n3, Joker
        | Num n1, Num n2, Joker, Num n3
        | Num n1, Joker, Num n2, Num n3
        | Joker, Num n1, Num n2, Num n3 ->
            n1.color != n2.color && n2.color != n3.color && n1.color != n3.color
        | Num n1, Num n2, Joker, Joker
        | Num n1, Joker, Joker, Num n2
        | Joker, Joker, Num n1, Num n2
        | Num n1, Joker, Num n2, Joker
        | Joker, Num n1, Joker, Num n2
        | Joker, Num n1, Num n2, Joker -> n1.color != n2.color
        | _ -> false)
    | _ -> false

  (** given a list of tiles of the same color, checks that they are in
      consecutive off-by-one order*)
  let rec check_rownums (tlst : tile list) : bool =
    match tlst with
    | [] -> true
    | _ :: [] -> true
    | [ h1; h2 ] -> (
        match (h1, h2) with
        | Num n1, Num n2 -> n2.num == n1.num + 1
        | _ -> true)
    | [ h1; h2; h3 ] -> (
        match (h1, h2, h3) with
        | Num n1, Num n2, Num n3 -> n2.num == n1.num + 1 && n3.num == n2.num + 1
        | Num n1, Joker, Num n2 -> n2.num == n1.num + 2
        | Num n1, Num n2, Joker | Joker, Num n1, Num n2 -> n2.num == n1.num + 1
        | _ -> true)
    | h1 :: h2 :: h3 :: h4 :: t -> (
        match (h1, h2, h3, h4) with
        | Joker, Joker, Num _, _ -> check_rownums (h3 :: h4 :: t)
        | Joker, Num n1, Num n2, _ ->
            if n2.num == n1.num + 1 then check_rownums (h3 :: h4 :: t)
            else false
        | Num n1, Joker, Num n2, _ ->
            if n2.num == n1.num + 2 then check_rownums (h3 :: h4 :: t)
            else false
        | Num n1, Num n2, Joker, Num n3 ->
            if n2.num == n1.num + 1 && n3.num == n2.num + 2 then
              check_rownums (h4 :: t)
            else false
        | Num n1, Num n2, Joker, Joker ->
            if n2.num == n1.num + 1 then check_rownums (h3 :: h4 :: t)
            else false
        | Num n1, Joker, Joker, Num n3 ->
            if n3.num == n1.num + 3 then check_rownums (h4 :: t) else false
        | Joker, Num n1, Joker, Num n2 ->
            if n2.num == n1.num + 2 then check_rownums (h4 :: t) else false
        | Num n1, Num n2, Num n3, _ -> check_rownums (h4 :: t)
        | _ -> false)

  (** finds the color of the first non-Joker tile, returns: (false, Yellow) if
      no color found, otherwise (true, color)*)
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

  (** finds the number of the first non-Joker tile, returns: (false, 0) if no
      number found, otherwise (true, num)*)
  let get_num (t1 : tile) (t2 : tile) (t3 : tile) : bool * int =
    match t1 with
    | Joker -> (
        match t2 with
        | Joker -> (
            match t3 with
            | Joker -> (false, 0)
            | Num n3 -> (true, n3.num))
        | Num n2 -> (true, n2.num))
    | Num n1 -> (true, n1.num)

  (** checks if the list of tiles is valid given a color and number*)
  let rec valid_row (c : color) (n : int) (tlst : tile list) : bool =
    if check_color c tlst then check_rownums tlst
    else if check_num n tlst then check_rowcolors tlst
    else false

  let rec check (board : tile list list) : bool =
    match board with
    | [] -> true
    | h :: t -> (
        if List.length h < 3 && List.length h != 0 then false
        else
          match h with
          | [] -> true
          | _ :: [] -> false
          | [ _; _ ] -> false
          | [ h1; h2; h3 ] -> (
              let l = [ h1; h2; h3 ] in
              let c = get_color h1 h2 h3 in
              let n = get_num h1 h2 h3 in
              match (c, n) with
              | (b1, col), (b2, number) ->
                  if b1 && b2 then valid_row col number l && check t else false)
          | [ h1; h2; h3; h4 ] -> (
              let l = [ h1; h2; h3; h4 ] in
              let c = get_color h1 h2 h3 in
              let n = get_num h1 h2 h3 in
              match (c, n) with
              | (b1, col), (b2, number) ->
                  if b1 && b2 then valid_row col number l && check t else false)
          | h1 :: h2 :: h3 :: tlist -> (
              let c = get_color h1 h2 h3 in
              match c with
              | b, col ->
                  if b then
                    let l = h1 :: h2 :: h3 :: tlist in
                    if check_color col l then check_rownums l && check t
                    else false
                  else false))

  (** Helper for check_first, returns sum of tiles in row*)
  let rec count_tiles (lst : tile list) : int =
    match lst with
    | [] -> 0
    | h :: t -> (
        match h with
        | Num n -> n.num + count_tiles t
        | Joker -> 10 + count_tiles t)

  let rec check_first (board : tile list list) : bool =
    if check board then
      let rec tiles b =
        match b with
        | [] -> 0
        | h :: t -> count_tiles h + tiles t
      in
      tiles board >= 30
    else false
end

(** A GameType represents the whole state of a Rummikaml game *)
module type GameType = sig
  val next_player : game_state -> game_state
  val check_win : player -> bool
  val make : string list -> string list -> game_state
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

  (* Function checks if given player has won (no more tiles) *)
  let check_win (p : player) : bool = p.hand = []

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
      returns tuple of element removed and modified list. Requires n is index
      within bounds of given list. *)
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
      let num = Random.int (List.length lst) in
      let popped = pop num lst in
      let lsts = subset (n - 1) (snd popped) in
      (fst popped :: fst lsts, snd lsts)

  (** Helper for make function that distributes 14 random starting tiles from
      the deck to each player - returns tuple of player list with hands and
      remaining deck *)
  let rec distribute_hand (players : player list) (deck : tile list) :
      player list * tile list =
    match players with
    | [] -> ([], deck)
    | h :: t ->
        let s = subset 14 deck in
        let d = distribute_hand t (snd s) in
        ({ h with hand = fst s } :: fst d, snd d)

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
  let make (players : string list) (cpus : string list) : game_state =
    let p =
      List.map (fun n -> { hand = []; name = n; isCpu = false }) players
    in
    let c = List.map (fun n -> { hand = []; name = n; isCpu = true }) cpus in
    let dis = distribute_hand (p @ c) (create_deck 13 []) in
    choose_first_player { players = fst dis; board = []; deck = snd dis }

  let active_player (state : game_state) = List.hd state.players
end
