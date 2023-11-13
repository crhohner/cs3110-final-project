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
let check_threes l = failwith "unimplmeneted"

let in_seq t1 t2 =
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
        if in_seq tile h then aux ([ tile; h ] :: acc) t
        else if in_seq h tile then aux ([ h; tile ] :: acc) t
        else aux acc t
  in
  aux [] lst

let check_pairs (tiles : tile list) : tile list list =
  let rec aux acc = function
    | h :: t -> aux (add_multi acc (check_pairs_aux h t)) t
    | [] -> acc
  in
  TileSet.elements (aux TileSet.empty tiles)

let place_three b l = failwith "unimplemented"
let place_pair b l = failwith "unimplemented"
let place_one b t = failwith "unimplemented"
let turn p b = failwith "unimplemented"
