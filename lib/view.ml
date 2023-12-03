open Model

module type ViewType = sig
  val show_board : game_state -> unit
  val clear : int -> unit
  val show_hand : player -> unit
  val show_win : game_state -> unit
  val show_turn : game_state -> bool -> unit
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
    let rec aux (row : tile list) (acc : string) (ntile : int) =
      match row with
      | h :: [] -> acc ^ string_of_int ntile ^ ":" ^ string_of_tile h
      | h :: t ->
          let acc = acc ^ string_of_int ntile ^ ":" ^ string_of_tile h ^ " " in
          aux t acc (ntile + 1)
      | [] -> acc
    in
    aux row "" 0

  let show_board (state : game_state) : unit =
    match state.board with
    | [] -> print_endline "the board is empty"
    | _ ->
        List.iteri
          (fun i row ->
            print_endline (string_of_int i ^ " | " ^ string_of_row row))
          state.board

  let rec clear (lines : int) : unit =
    match lines with
    | 0 -> ()
    | _ ->
        let _ = print_newline () in
        clear (lines - 1)

  let show_hand (player : player) : unit =
    print_endline (string_of_row player.hand)

  (**Returns a string with [n] repeats of [char]. *)
  let rec make_repeats char n =
    let rec aux n acc = if n = 0 then acc else aux (n - 1) char ^ acc in
    aux n ""

  (**Prints a bar that is at least 80 characters long to separate CLI sections.
     If the longest row on the board if longer than 80 characters, prints a bar
     of that length, otherwise prints an 80-character bar.*)
  let print_bar (state : game_state) =
    (* len always evaluates to 0 *)
    let len =
      let p = Game.active_player state in
      String.length (string_of_row p.hand)
    in
    let count = if len < 82 then 82 else len in
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

  (* check is the sum of the move and deck length*)
  let show_actions (game : game_state) (altered : bool) () =
    let msg =
      "actions: add to row (a) | help (h)"
      ^ (if altered = true then " | end turn (e)" else "")
      ^ (if altered = false && game.deck <> [] then " | draw from deck (d)"
         else "")
      ^ (match game.board with
        | [ [ _ ] ] -> ""
        | [] -> ""
        | _ -> " | move tile (m)")
      ^ if altered then " | reset turn (r)" else ""
    in

    print_endline msg

  let show_turn (state : game_state) (altered : bool) : unit =
    print_bar state;
    print_hand_sizes state;
    print_bar state;
    show_board state;
    let active = Game.active_player state in
    print_newline ();
    print_endline (active.name ^ "'s hand:");
    show_hand active;
    print_bar state;
    show_actions state altered ();
    print_bar state

  let show_win (state : game_state) =
    show_board state;
    print_endline
      (let player = Game.active_player state in
       player.name ^ " wins!")

  let show_help () =
    print_endline
      "help: \n\
      \ \t - Rules: \n\
      \ \t - For a valid row, there must be at least 3 tiles in that row \n\
      \ \t - Each row can either have \n\
      \ \t \t - the same colored tiles, increasing by 1 (there are 13 numbers)\n\
      \ \t \t - or the same number with all different colors (there are 4 \
       colors) \n\
      \ \t - Jokers count as any tile (there are only 2 in the game) \n\
      \ \t - Your goal is to get rid of all your tiles! \n\
      \ \t - If you have no valid moves, draw a tile from the deck \n\
      \ \t - Mechanics: \n\
      \ \t - add to row (a): add a tile from your hand to the board \n\
      \ \t - move tile (m): move a tile on the board \n\
      \ \t - draw from deck (d): draw a tile, ends your turn \n\
      \ \t - reset turn (r): reset your current turn \n\
      \ \t - end turn (e): end your current turn \n\n\
       rules: visit https://rummikub.com/rules/\n"
end
