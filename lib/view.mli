open Model

(*implemented as CLI in model.ml, later maybe also GUI ?? >:) *)
module type View = sig
  val show_board : 'a list -> _

  (*displays so many newlines, n num*)
  val clear : int -> _

  (*displays hand of player*)
  val show_hand : player -> _

  (* lil message*)
  val show_win : game_state -> _

  (*print healp*)
  val show_help : _
end
