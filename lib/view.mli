open Model

(*implemented as CLI in model.ml, later maybe also GUI ?? >:) *)
module type ViewType = sig
  val show_board : game_state -> unit
  val clear : int -> unit
  val show_hand : player -> unit
  val show_win : game_state -> player -> unit
  val show_help : unit -> unit
  val string_of_tile : tile -> string
end

module CLIPrinter (Game : GameType) : ViewType
