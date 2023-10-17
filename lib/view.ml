open Model

module type ViewType = sig
  val show_board : game_state -> unit
  val clear : int -> unit
  val show_hand : player -> unit
  val show_win : game_state -> player -> unit
  val show_turn : game_state -> unit
  val show_help : unit -> unit
  val string_of_tile : tile -> string
  val string_of_row : tile list -> string
end

(**An implementation of ViewType which prints information about the game to the
   CLI*)
module CLIPrinter : ViewType = struct
  let string_of_tile (tile : tile) =
    match tile with
    | Joker -> "[JJ]"
    | Num n ->
        let color =
          match n.color with
          | Yellow -> "Y"
          | Red -> "R"
          | Black -> "K"
          | Blue -> "B"
        in
        "[" ^ string_of_int n.num ^ color ^ "]"

  let string_of_row (row : tile list) : string =
    let rec aux (row : tile list) (acc : string) =
      match row with
      | h :: [] -> acc ^ string_of_tile h
      | h :: t ->
          let acc = acc ^ string_of_tile h ^ " " in
          aux t acc
      | [] -> raise (Failure "what even")
    in
    aux row ""

  let show_board (state : game_state) : unit =
    match state.board with
    | [] -> print_endline "the board is empty"
    | _ ->
        List.iteri
          (fun i row ->
            print_endline ("row " ^ string_of_int i ^ string_of_row row))
          state.board

  let rec clear (lines : int) : unit =
    match lines with
    | 0 -> ()
    | _ ->
        let _ = print_newline in
        clear (lines - 1)

  let show_hand (player : player) : unit =
    print_endline (string_of_row player.hand)

  (**Returns the length of the string representation of the longest row (in
     terms of its representation) on the board [board].*)
  let rec longest_row board =
    List.fold_left
      (fun max row ->
        let check = String.length (string_of_row row) in
        if check > max then check else max)
      0 board

  (**Returns a string with [n] repeats of [char]. *)
  let rec make_repeats char n =
    let rec aux n acc = if n = 0 then acc else aux (n - 1) char ^ acc in
    aux n ""

  (**Prints a bar that is at least 80 characters long to separate CLI sections.
     If the longest row on the board if longer than 80 characters, prints a bar
     of that length, otherwise prints an 80-character bar.*)
  let print_bar (state : game_state) =
    let len = longest_row state.board in
    let count = if len < 80 then 80 else len in
    print_endline (make_repeats "—" count)

  (**Displays how many tiles every player has in their hands except for how many
     tiles the active player has.*)
  let print_hand_sizes (state : game_state) =
    let _ = print_endline "hands:" in
    let rec aux players acc =
      match players with
      | h :: t ->
          aux t acc ^ " | " ^ h.name ^ ": "
          ^ (List.length h.hand |> string_of_int)
      | [] -> acc
    in
    match state.players with
    | h :: [] -> raise (Failure "1 or fewer players")
    | [] -> raise (Failure "1 or fewer players")
    | _ :: t -> print_endline (aux t "")

  let show_actions () =
    print_endline
      "actions: add to row (a) | start new row (n) | move (m) | end turn (e) | \
       help (h) "

  let show_turn (state : game_state) : unit =
    print_bar state;
    print_hand_sizes state;
    print_bar state;
    show_board state;
    let active = Game.active_player state in
    print_newline ();
    print_endline (active.name ^ "'s hand:");
    show_hand active;
    print_bar state;
    show_actions ();
    print_bar state

  let show_win (state : game_state) (player : player) =
    print_endline (player.name ^ " wins!")

  let show_help () = failwith "u"
end
