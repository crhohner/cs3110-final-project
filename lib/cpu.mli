open Model

val sort_by_num : tile list -> (int * tile list) list
(** Returns cpu's hand as association list where the keys are tile numbers
    (Joker is 0) and the values are lists of the associated tiles (with
    duplicates removed, excluding Jokers). **)

val sort_by_color : tile list -> (color option * tile list) list
(** Returns cpu's hand as association list where the keys are color options
    (Joker is None) and the values are lists of the associated tiles (with
    duplicates removed, excluding Jokers, and sorted in ascending order by
    number). **)

val check_threes : tile list -> tile list option
(** Returns an option containing a valid, ordered sequence of three tiles from a
    tile list, or [None] if no such sequence can be made. Will use two jokers in
    one turn if necessary. **)

val check_pairs : tile list -> tile list list
(** Returns a list of all in-sequence pairs of 2 tiles from a list of tiles.
    Makes sure there are no duplicate pairs.**)

val place_three : tile list list -> tile list -> tile list list
(** Returns a board with a sequence of three tiles added to it as a new row.
    Requires: the sequence of tiles follows either the ascending, same color
    rule or the same number, different color rule, and has at most two Jokers. **)

val place_pair : tile list list -> tile list -> tile list list
(** Returns a board with a pair of tiles from [l] placed down at a legal
    location. If no pair can be placed, returns AN UNALTERED BOARD.*)

val place_one : tile list list -> tile -> tile list list
(** Returns a board with a tile from [l] placed down at a legal location. If no
    tile can be placed, returns AN UNALTERED BOARD.*)

val turn : game_state -> game_state
(** Returns the state of a CPU player alongside an altered board after one CPU
    turn. The CPU cannot move tiles, but will place down any tiles it can
    otherwise. **)
