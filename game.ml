(* Game *)
(* $Id: game.ml,v 1.2 2003/11/18 22:51:09 berke Exp $ *)
(* vim:set ts=4: *)
(* By Oguz Berke DURAK *)

open Human

module type S =
  sig
    type configuration
    type state
    val default_configuration : configuration
    val pixel_dimensions : configuration -> int * int
    val initial_state : configuration -> state
    val transition : configuration -> state -> float -> Keypad.event option -> Display.command list * float option
  end
