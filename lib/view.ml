open Model

module type ViewType = sig
  val show_board : game_state -> unit
  val clear : int -> unit
  val show_hand : player -> unit
  val show_win : game_state -> player -> unit
  val show_help : unit -> unit
  val string_of_tile : tile -> string
end

(**An implementation of ViewType which prints information about the game to the
   CLI*)
module CLIPrinter (Game : GameType) : ViewType = struct
  let show_board (state : game_state) : unit = failwith "u"
  let rec clear (lines : int) : unit = failwith "u"

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

  let show_hand (player : player) : unit =
    let rec string_of_hand (hand : tile list) (acc : string) =
      match hand with
      | h :: [] -> acc ^ string_of_tile h
      | h :: t ->
          let acc = acc ^ string_of_tile h ^ " " in
          string_of_hand t acc
      | [] -> raise (Failure "what even")
    in
    print_endline (string_of_hand player.hand "")

  let show_win (state : game_state) (player : player) = failwith "u"
  let show_help () = failwith "u"
end
