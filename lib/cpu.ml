open Model

module Ord : Set.OrderedType with type t = tile list = struct
  type t = tile list

  let rec sum = function
    | [] -> 0
    | h :: t -> num_of_tile h + sum t

  let compare l1 l2 = sum l1 - sum l2
end

module TileSet : Set.S with type elt = tile list = Set.Make (Ord)

let rec add_multi set lst =
  match lst with
  | [] -> set
  | h :: t -> add_multi (TileSet.add h set) t

(* Helper of sort_by_num *)
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

(* Helper of sort_by_color *)
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

let pair_in_seq t1 t2 =
  match (t1, t2) with
  | Joker, Num n -> true
  | Num n, Joker -> true
  | Joker, Joker -> true
  | Num { color = c1; num = n1 }, Num { color = c2; num = n2 } ->
      (c1 <> c2 && n1 = n2) || (c1 = c2 && n1 + 1 = n2)

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
  TileSet.elements (aux TileSet.empty tiles)

(*checks for ascending sequences within a tile list*)
let rec find_num_seq (l : tile list) : tile list option =
  match l with
  | Num n1 :: Num n2 :: Num n3 :: t ->
      if n1.num + 2 = n3.num && n2.num + 1 = n3.num then
        Some [ Num n1; Num n2; Num n3 ]
      else find_num_seq (Num n2 :: Num n3 :: t)
  | _ -> None

(*returns set of three tiles within a tile list *)
let rec find_color_seq (l : tile list) : tile list option =
  match l with
  | Num n1 :: Num n2 :: Num n3 :: t -> Some [ Num n1; Num n2; Num n3 ]
  | _ -> None

(*checks for ascending sequences within the output of sort_by_num assuming no
  jokers present*)
let rec check_seqs_by_num (l : (int * tile list) list) : tile list option =
  match l with
  | (k, v) :: t -> (
      match find_num_seq v with
      | Some lst -> Some lst
      | None -> check_seqs_by_num t)
  | [] -> None

(*checks for some sequence using a given find method*)
let rec check_seqs find (l : ('a * tile list) list) : tile list option =
  match l with
  | (k, v) :: t -> (
      match find v with
      | Some lst -> Some lst
      | None -> check_seqs find t)
  | _ -> None

(*find stuff that works with jokers*)
(*single joker*)
let rec find_num_pair_j (l : tile list) : tile list option =
  match l with
  | Num n1 :: Num n2 :: t ->
      if n1.num + 2 = n2.num then Some [ Num n1; Joker; Num n2 ]
      else if n1.num + 1 = n2.num then Some [ Num n1; Num n2; Joker ]
      else find_num_seq (Num n2 :: t)
  | _ -> None

let rec find_color_pair_j (l : tile list) : tile list option =
  match l with
  | Num n1 :: Num n2 :: t -> Some [ Num n1; Num n2; Joker ]
  | _ -> None

(*finds first non-Joker tile*)
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
  match (check_seqs find_num_seq n, check_seqs find_color_seq c) with
  | Some out, None -> Some out
  | None, Some out -> Some out
  | Some o1, Some o2 -> Some o1
  | _ ->
      if joker_count > 0 then
        match
          (check_seqs find_num_pair_j n, check_seqs find_color_pair_j c)
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

(** Helper function for color_seq. Returns true iff all the tiles in a list of
    colors are composed of colors that differ from one another. Jokers are
    disregarded in determining the returned boolean. *)
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

let rec find_valid_pair (b : tile list list) (l : tile list list) =
  match l with
  | [] -> b
  | h :: t ->
      let new_board = place_pair b h in
      if new_board = b then find_valid_pair b t else new_board

let rec place_one_rec (b : tile list list) (l : tile list) =
  match l with
  | [] -> b
  | h :: t ->
      let new_board = place_one b h in
      if new_board = b then place_one_rec b t else new_board

let rec turn p b =
  let hand = p.hand in
  let threes = check_threes hand in
  match threes with
  | Some l ->
      let new_board = place_three b l in
      (*update hand here and put that as updated player in turn call - get
        indices of the tiles and remove them from hand*)
      turn p new_board
  | None ->
      let pairs = check_pairs hand in
      let new_board = find_valid_pair b pairs in
      if new_board = b then
        let new_board = place_one_rec b hand in
        if new_board = b then (p, new_board)
        else
          (*update hand here and put that as updated player in turn call - get
            indices of the tiles and remove them from hand*) (p, new_board)
      else
        (*update hand here and put that as updated player in turn call - get
          indices of the tiles and remove them from hand*) turn p new_board
