(* Tetris *)
(* $Id: tetris.mli,v 1.2 2003/11/18 22:51:09 berke Exp $ *)

open Human

type configuration
type state
type input = Left | Right | Rotate | Rotate' | Down | Drop | Pause | Quit
val default_configuration : configuration
val pixel_dimensions : configuration -> int * int
val initial_state : configuration -> state
