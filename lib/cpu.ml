open Model

module Ord : Set.OrderedType with type t = tile list = struct
  type t = tile list

  let rec sum = function
    | [] -> 0
    | h :: t -> num_of_tile h + sum t

  let compare l1 l2 =
    if l1 <> l2 then if sum l1 - sum l2 = 0 then 1 else sum l1 - sum l2 else 0
end

(*TileListSets guarantee that no two lists in a set have the same tiles,
  regardless of order.*)
module TileListSet : Set.S with type elt = tile list = Set.Make (Ord)

(*Returns [set] with each element in [lst] inserted.*)
let rec add_multi set lst =
  match lst with
  | [] -> set
  | h :: t -> add_multi (TileListSet.add h set) t

(* Helper of sort_by_num. Determines whether tile list contains tile with given
   color. If yes, returns true, else returns false. *)
let rec find_c (c : color) (lst : tile list) =
  match lst with
  | [] -> false
  | h :: t -> (
      match h with
      | Num n -> if n.color = c then true else find_c c t
      | _ -> false)

let sort_by_num l =
  List.fold_left
    (fun acc t ->
      match t with
      | Joker ->
          if not (List.mem_assoc 0 acc) then (0, [ t ]) :: acc
          else (0, [ t; t ]) :: List.remove_assoc 0 acc
      | Num n ->
          if not (List.mem_assoc n.num acc) then (n.num, [ t ]) :: acc
          else
            let s = List.assoc_opt n.num acc in
            let present = find_c n.color (Option.get s) in
            if present then acc
            else (n.num, t :: Option.get s) :: List.remove_assoc n.num acc)
    [] l

(* Helper of sort_by_color. Determines whether tile list contains tile with
   given number. If yes, returns true, else returns false. *)
let rec find_n (n : int) (lst : tile list) =
  match lst with
  | [] -> false
  | h :: t -> (
      match h with
      | Num nm -> if nm.num = n then true else find_n n t
      | _ -> false)

let sort_by_color l =
  let lst =
    List.fold_left
      (fun acc t ->
        match t with
        | Joker ->
            if not (List.mem_assoc None acc) then (None, [ t ]) :: acc
            else (None, [ t; t ]) :: List.remove_assoc None acc
        | Num n ->
            if not (List.mem_assoc (Some n.color) acc) then
              (Some n.color, [ t ]) :: acc
            else
              let s = List.assoc_opt (Some n.color) acc in
              let present = find_n n.num (Option.get s) in
              if present then acc
              else
                (Some n.color, t :: Option.get s)
                :: List.remove_assoc (Some n.color) acc)
      [] l
  in
  List.map
    (fun (x, t_lst) ->
      let lst =
        List.sort
          (fun t1 t2 ->
            match (t1, t2) with
            | Num n1, Num n2 -> n1.num - n2.num
            | _ -> 0)
          t_lst
      in
      (x, lst))
    lst

(*Returns whether [t1] and [t2] form a valid ordered, sequence. Following
  Rummikaml rules, this is true if a) [t1] and [t2] are the same color and the
  number of [t2] is one more than the number of [t1], or b) [t1] and [t2] have
  the same number but different colors.*)
let pair_in_seq t1 t2 =
  match (t1, t2) with
  | Joker, Num n -> true
  | Num n, Joker -> true
  | Joker, Joker -> true
  | Num { color = c1; num = n1 }, Num { color = c2; num = n2 } ->
      (c1 <> c2 && n1 = n2) || (c1 = c2 && n1 + 1 = n2)

(*Returns all in-sequence pairs that can be created from [tile] and another tile
  in [lst]. Does not guarantee unique pairs.*)
let check_pairs_aux (tile : tile) (lst : tile list) : tile list list =
  let rec aux (acc : tile list list) (l : tile list) : tile list list =
    match l with
    | [] -> acc
    | h :: t ->
        if pair_in_seq tile h then aux ([ tile; h ] :: acc) t
        else if pair_in_seq h tile then aux ([ h; tile ] :: acc) t
        else aux acc t
  in
  aux [] lst

let check_pairs (tiles : tile list) : tile list list =
  let rec aux acc = function
    | h :: t -> aux (add_multi acc (check_pairs_aux h t)) t
    | [] -> acc
  in
  TileListSet.elements (aux TileListSet.empty tiles)

(*the two functions below are used to find all sequences from the two sort
  outcomes that do not require a joker. *)

(*start here*)
(*Returns a sequence of three ascending tiles given a list of tiles, or [None]
  if no such sequence exists. Requires: tiles in [l] are all the same color and
  sorted in ascending order, and there are no duplicates in [l].*)
let rec find_num_seq (l : tile list) : tile list option =
  match l with
  | Num n1 :: Num n2 :: Num n3 :: t ->
      if n1.num + 2 = n3.num && n2.num + 1 = n3.num then
        Some [ Num n1; Num n2; Num n3 ]
      else find_num_seq (Num n2 :: Num n3 :: t)
  | _ -> None

(*Returns an option containing sequence of three tiles from a list of tiles [l],
  or [None] if [l] is too short. In context, [find_color_seq] requires that [l]
  contains no duplicate elements and that every tiles in [l] has the same
  number.*)
let rec find_color_seq (l : tile list) : tile list option =
  match l with
  | Num n1 :: Num n2 :: Num n3 :: t -> Some [ Num n1; Num n2; Num n3 ]
  | _ -> None

(*Returns an option containing an ordered sequence of three tiles matching a
  certain rule defined by [find] from the outcome of a sort, or [None] if no
  such sequence can be found.*)
let rec check_seqs find (l : ('a * tile list) list) : tile list option =
  match l with
  | (k, v) :: t -> (
      match find v with
      | Some lst -> Some lst
      | None -> check_seqs find t)
  | _ -> None

(*Returns an option containing a three-tile ascending sequence that can be made
  from two tiles in [l] and a single joker or [None] if no such sequence can be
  found.*)
let rec find_num_pair_j (l : tile list) : tile list option =
  match l with
  | Num n1 :: Num n2 :: t ->
      if n1.num + 2 = n2.num then Some [ Num n1; Joker; Num n2 ]
      else if n1.num + 1 = n2.num && n2.num <> 13 then
        Some [ Num n1; Num n2; Joker ]
      else if n1.num + 1 = n2.num && n1.num <> 1 then
        Some [ Joker; Num n1; Num n2 ]
      else find_num_pair_j (Num n2 :: t)
  | _ -> None

(*Returns an option containing a three-tile same number, different color
  sequence that can be made from two tiles in [l] and a single joker or [None]
  if no such sequence can be found.*)
let rec find_color_pair_j (l : tile list) : tile list option =
  match l with
  | Num n1 :: Num n2 :: t -> Some [ Num n1; Num n2; Joker ]
  | _ -> None

(*Returns the first tile in [l] that is not a joker.*)
let rec find_first_num (l : tile list) =
  match l with
  | [] -> None
  | h :: t -> (
      match h with
      | Joker -> find_first_num t
      | Num n -> Some (Num n))

let check_threes l =
  let n = sort_by_num l in
  let c = sort_by_color l in
  let joker_count =
    match List.assoc_opt 0 n with
    | Some s -> List.length s
    | None -> 0
  in
  match (check_seqs find_num_seq c, check_seqs find_color_seq n) with
  | Some out, None -> Some out
  | None, Some out -> Some out
  | Some o1, Some o2 -> Some o1
  | _ ->
      if joker_count > 0 then
        match
          (check_seqs find_num_pair_j c, check_seqs find_color_pair_j n)
        with
        | Some out, None -> Some out
        | None, Some out -> Some out
        | Some o1, Some o2 -> Some o1
        | _ -> (
            match find_first_num l with
            | None -> None
            | Some n ->
                if joker_count = 2 then Some [ n; Joker; Joker ] else None)
      else None

let place_three b l = b @ [ l ]

let rec place_pair b l =
  let tile1 = List.nth l 0 in
  let tile2 = List.nth l 1 in
  match b with
  | [] -> []
  | h :: t ->
      if
        check_threes [ tile1; tile2; List.hd h ]
        = Some [ tile1; tile2; List.hd h ]
      then (l @ h) :: t
      else if
        check_threes [ List.hd (List.rev h); tile1; tile2 ]
        = Some [ List.hd (List.rev h); tile1; tile2 ]
      then (h @ l) :: t
      else h :: place_pair t l

(** Returns iff all the tiles in a list of colors are composed of colors that
    differ from one another. Jokers are disregarded in determining the returned
    boolean. Helper function for color_seq.*)
let rec color_vary l c =
  let rec color_mem (c : color) = function
    | [] -> false
    | h :: t -> if h = c then true else color_mem c t
  in
  match (l, c) with
  | h :: t, [] -> (
      match h with
      | Joker -> color_vary t []
      | Num n -> color_vary t [ n.color ])
  | h :: t, c -> (
      match h with
      | Joker -> color_vary t c
      | Num n ->
          if color_mem n.color c then false else color_vary t (n.color :: c))
  | [], [] -> true (* should never happen *)
  | [], c -> true

(** Helper function for place_one that addresses the corner case in which a tile
    is potentially to be placed in a three-tile sequence of cards. If the
    sequence of tiles is a sequence of same-number, color-varied tiles in which
    the tile can be legally placed, this function returns true. Assumes the
    sequence of tile is legal such that either all tiles are of the same number
    or they all vary. *)
let color_seq (l : tile list) (t : tile) =
  let rec num_check l t =
    match t with
    | Joker -> true
    | Num n -> (
        match List.hd l with
        | Joker -> num_check (List.tl l) t
        | Num n1 -> n.num = n1.num)
  in
  color_vary (t :: l) [] && num_check l t

let rec place_one b t =
  match b with
  | [] -> []
  | h :: tl ->
      let rev_h = List.rev h in
      if
        check_threes [ t; List.hd h; List.nth h 1 ]
        = Some [ t; List.hd h; List.nth h 1 ]
      then (t :: h) :: tl
      else if
        check_threes [ List.nth rev_h 1; List.hd rev_h; t ]
        = Some [ List.nth rev_h 1; List.hd rev_h; t ]
      then (h @ [ t ]) :: tl
      else if List.length h = 3 && color_seq h t then (t :: h) :: tl
      else h :: place_one tl t

(**Returns a board with a pair of tiles from [l] added to it alongide that pair.
   If no pair can be added, returns an unaltered board and ([]). Requires: [b]
   is a legal board, [l] contains only in-sequence pairs. **)
let rec find_valid_pair (b : tile list list) (l : tile list list) =
  match l with
  | [] -> (b, [])
  | h :: t ->
      let new_board = place_pair b h in
      if new_board = b then find_valid_pair b t else (new_board, h)

(**Returns a board with a tile from [l] added to it alongide an option
   containing that tile. If no tile from [l] can be added, returns an unaltered
   board and [None]. Requires: [b] is a legal board. **)
let rec place_one_rec (b : tile list list) (l : tile list) =
  match l with
  | [] -> (b, None)
  | h :: t ->
      let new_board = place_one b h in
      if new_board = b then place_one_rec b t else (new_board, Some h)

(**Returns the first index of [t] in [lst], or -1 if [t] is not in [lst].**)
let rec find_tile (lst : 'a list) (t : 'a) acc =
  match lst with
  | [] -> -1
  | h :: tail -> if h = t then acc else find_tile tail t (acc + 1)

(**Removes the first instance of each item in [l] from [hand].**)
let rec remove_tiles (hand : 'a list) (l : 'a list) =
  match l with
  | [] -> hand
  | h :: t -> remove_tiles (snd (remove (find_tile hand h 0) hand)) t

let rec turn game_state : game_state =
  let p = Game.active_player game_state in
  let b = game_state.board in
  let d = game_state.deck in
  let hand = p.hand in
  let threes = check_threes hand in
  match threes with
  | Some l ->
      let new_board = place_three b l in
      let new_hand = remove_tiles hand l in
      let p' = { p with hand = new_hand } in
      let new_game_state =
        {
          players = p' :: List.tl game_state.players;
          board = new_board;
          deck = d;
        }
      in
      turn new_game_state
  | None ->
      let pairs = check_pairs hand in
      let new_board = fst (find_valid_pair b pairs) in
      if new_board = b then
        let new_board, t = place_one_rec b hand in
        match t with
        | None ->
            let rand_tile = d |> List.length |> Random.int in
            let drawn, new_deck = remove rand_tile d in
            let new_game_state =
              {
                players =
                  List.tl game_state.players
                  @ [ { p with hand = drawn :: p.hand } ];
                board = new_board;
                deck = new_deck;
              }
            in
            new_game_state
        | Some tile ->
            let new_hand = remove_tiles hand [ tile ] in
            let p' = { p with hand = new_hand } in
            let new_game_state =
              {
                players = List.tl game_state.players @ [ p' ];
                board = new_board;
                deck = d;
              }
            in
            new_game_state
      else
        let new_hand = remove_tiles hand (snd (find_valid_pair b pairs)) in
        let p' = { p with hand = new_hand } in
        let new_game_state =
          {
            players = p' :: List.tl game_state.players;
            board = new_board;
            deck = d;
          }
        in
        turn new_game_state
