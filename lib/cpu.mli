open Model

val sort_by_num : tile list -> tile list list
(** Returns cpu's hand as a list of tile lists sorted by number **)

val sort_by_color : tile list -> tile list list
(** Returns cpu's hand as a list of tile lists sorted by color - lists
    themselves are sorted in ascending order by number**)

val check_threes : tile list -> tile list option
(** Checks cpu's hand for possible valid set of 3 tiles. Returns [None] if no
    valid set exists and [Some v] otherwise**)

val check_pairs : tile list -> tile list list
(** Checks cpu's hand for possible valid set of 2 tiles. Returns list of all
    valid pairs of tiles. Makes sure there are no duplicate pairs**)

val place_three : tile list list -> tile list -> tile list list
(** Takes board and valid set of 3 tiles in cpu's hand - places set down in
    board on new row and returns modified board**)

val place_pair : tile list list -> tile list -> tile list list
(** Takes board and valid pair of tiles in cpu's hand - places pair down if
    possible and returns modified board**)

val place_one : tile list list -> tile -> tile list list
(** Takes board and tile in cpu's hand - places tile down if possible and
    returns modified board**)

val turn : player -> tile list list -> player * tile list list
(** Completes a cpu turn**)
