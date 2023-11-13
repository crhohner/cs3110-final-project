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

let sort_by_num l = failwith "unimplemented"
let sort_by_color l = failwith "unimplemented"

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
  | lst -> three_colors first_nums

let check_threes l =
  if List.length l < 3 then None
  else
    let sorted_num_l = sort_by_num l in
    let colors = nums_list sorted_num_l in
    match colors with
    | None -> (
        let sorted_color_l = sort_by_color l in
        let nums = three_nums sorted_color_l in
        match nums with
        | None -> None
        | _ -> nums
        (*need to make sure it only goes for length of same color for each
          color*))
    | _ -> colors

let place_three b l = b @ [ l ]
let place_pair b l = failwith "unimplemented"
let place_one b t = failwith "unimplemented"
let turn p b = failwith "unimplemented"
