(* MAIN GAME LOOP, GRABS DATA FROM USER, PASSES TO MODEL, GETS STATE FROM MODEL,
   PASSES TO VIEW, PUTS OUT VIEW*)

(*main method goes here, user input goes here*)

module type Controller = sig
  val get_turn : string -> string option
  (**idek get turn, get names, yeah.*)
  (*returns Player<N> if invalid*)
  val get_name : string -> int -> string
  (*for input after win is shown*)
  val get_quit_or_new: string -> string option
end
