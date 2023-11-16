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

let sort_by_num l =
  let lst =
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
              (n.num, t :: Option.get s) :: List.remove_assoc n.num acc)
      [] l
  in
  snd (List.split lst)

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
              (Some n.color, t :: Option.get s)
              :: List.remove_assoc (Some n.color) acc)
      [] l
  in
  let lst2 = snd (List.split lst) in
  List.map
    (fun t_lst ->
      List.sort
        (fun t1 t2 ->
          match (t1, t2) with
          | Num n1, Num n2 -> n1.num - n2.num
          | _ -> 0)
        t_lst)
    lst2

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

let rec check_nums (l : tile list) =
  match l with
  | [] -> []
  | t1 :: t2 :: t3 :: tail -> (
      match (t1, t2, t3) with
      | Num n1, Num n2, Num n3 ->
          if n1.num == n2.num && n2.num == n3.num then
            t1 :: t2 :: check_nums (t3 :: tail)
          else []
      | Num n1, Joker, Num n2 | Joker, Num n1, Num n2 ->
          if n1.num == n2.num then t1 :: t2 :: check_nums (t3 :: tail) else []
      | Num n1, Num n2, Joker ->
          if n1.num == n2.num then t2 :: t3 :: check_nums (t1 :: tail) else []
      | Num n1, Joker, Joker -> t2 :: t3 :: check_nums (t1 :: tail)
      | Joker, Num n1, Joker -> t1 :: t3 :: check_nums (t2 :: tail)
      | _ -> [])
  | _ -> []

let rec three_colors (l : tile list) =
  match l with
  | t1 :: t2 :: t3 :: tail -> (
      match (t1, t2, t3) with
      | Num n1, Num n2, Num n3 ->
          if
            n1.color != n2.color && n2.color != n3.color && n1.color != n3.color
          then Some [ t1; t2; t3 ]
          else None
      | Joker, Num n1, Num n2 | Num n1, Joker, Num n2 | Num n1, Num n2, Joker ->
          if n1.color != n2.color then Some [ t1; t2; t3 ] else None
      | Joker, Joker, Num _ | Num _, Joker, Joker | Joker, Num _, Joker ->
          Some [ t1; t2; t3 ]
      | _ -> three_colors (t2 :: t3 :: tail))
  | _ -> None

let rec three_nums (l : tile list) =
  match l with
  | t1 :: t2 :: t3 :: tail -> (
      match (t1, t2, t3) with
      | Num n1, Num n2, Num n3 ->
          if n2.num == n1.num + 1 && n3.num == n2.num + 1 then
            Some [ t1; t2; t3 ]
          else None
      | Num n1, Joker, Num n2 ->
          if n2.num == n1.num + 2 then Some [ t1; t2; t3 ] else None
      | Num n1, Num n2, Joker | Joker, Num n1, Num n2 ->
          if n2.num == n1.num + 1 then Some [ t1; t2; t3 ] else None
      | _ -> None)
  | _ -> None

let rec nums_list (l : tile list) =
  let first_nums = check_nums l in
  match first_nums with
  | [] -> None
  | lst -> three_colors lst

let rec check_colors l =
  match l with
  | t1 :: t2 :: t3 :: tail -> (
      match (t1, t2, t3) with
      | Num n1, Num n2, Num n3 ->
          if n1.color = n2.color && n2.color = n3.color then [ t1; t2; t3 ]
          else check_colors (t2 :: t3 :: tail)
      | Joker, Num n1, Num n2 | Num n1, Joker, Num n2 | Num n1, Num n2, Joker ->
          if n1.color = n2.color then [ t1; t2; t3 ]
          else check_colors (t2 :: t3 :: tail)
      | Joker, Joker, Num n | Num n, Joker, Joker | Joker, Num n, Joker ->
          [ t1; t2; t3 ]
      | _ -> [])
  | _ -> []

let rec colors_check (l : tile list) : tile list option =
  let colors = check_colors l in
  match colors with
  | [] -> None
  | lst -> (
      let nums = three_nums lst in
      match nums with
      | None -> None
      | Some l -> Some l)

(** if List.length l < 3 then None else let sorted_num_l = sort_by_num l in let
    colors = nums_list sorted_num_l in match colors with | None -> let
    sorted_color_l = sort_by_color l in colors_check sorted_color_l | _ ->
    colors *)
let check_threes l = failwith "Unimplemented"

let rec check_num l num =
  match l with
  | [] -> []
  | h :: t -> (
      match h with
      | Num n -> if n.num == num then h :: check_num t num else []
      | Joker -> h :: check_num t num)

let rec num_check l acc =
  if acc == 13 then []
  else
    let lst = check_num l acc in
    match check_num l acc with
    | [] -> num_check lst (acc + 1)
    | nums -> nums

(** let check_threes_redo l = if List.length l < 3 then None else let sorted_num
    = sort_by_num l in let nums = num_check sorted_num 0 in if List.length nums
    < 3 then None else None *)

let place_three b l = b @ [ l ]

let rec place_pair b l = 
  let tile1 = List.nth l 0 in 
  let tile2 = List.nth l 1 in
  match b with 
  | [] -> []
  | h::t -> 
    if check_threes [tile1; tile2; List.hd h] = (Some [tile1; tile2; List.hd h]) 
      then (l @ h) :: t
    else if check_threes [List.hd (List.rev h); tile1; tile2;] = 
      (Some [List.hd (List.rev h); tile1; tile2;])
      then (h @ l) :: t
    else h::(place_pair t l)

(** Helper function for color_seq. Returns true iff all the tiles in a list of colors
    are composed of colors that differ from one another. Jokers are disregarded in
    determining the returned boolean. *)
let rec color_vary l c = 
  let rec color_mem (c : color) = function 
    | [] -> false
    | h::t -> if h = c then true else color_mem c t
  in match l, c with
  | h::t, [] -> (match h with
    | Joker -> color_vary t []
    | Num n -> color_vary t [n.color])
  | h::t, c -> (match h with
    | Joker -> color_vary t c
    | Num n -> if color_mem n.color c then false else color_vary t (n.color :: c))
  | [], [] -> true (* should never happen *)
  | [], c -> true

(** Helper function for place_one that addresses the corner case in which a tile 
    is potentially to be placed in a three-tile sequence of cards. If the sequence
    of tiles is a sequence of same-number, color-varied tiles in which the tile
    can be legally placed, this function returns true.
    Assumes the sequence of tile is legal such that either all tiles are of the
    same number or they all vary. *)
let color_seq (l : tile list) (t : tile) =
  let rec num_check l t =
    match t with
    | Joker -> true
    | Num n -> (match List.hd l with 
      | Joker -> (num_check (List.tl l) t)
      | Num n1 -> n.num = n1.num)
  in color_vary (t::l) [] && num_check l t

let rec place_one b t = 
  match b with
  | [] -> []
  | h::tl -> 
    let rev_h = List.rev h in
    if check_threes [t; List.hd h; List.nth h 1;] = 
      (Some [t; List.hd h; List.nth h 1;])
      then (t::h) :: tl
    else if check_threes [List.nth rev_h 1; List.hd rev_h; t] = 
      (Some [List.nth rev_h 1; List.hd rev_h; t])
      then (h @ [t])::tl
    else if List.length h = 3 && color_seq h t then (t::h)::tl
    else h::(place_one tl t)
let turn p b = failwith "unimplemented"
