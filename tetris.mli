(* Tetris *)
(* $Id: tetris.mli,v 1.2 2003/11/18 22:51:09 berke Exp $ *)

open Color

type input = Left | Right | Rotate | Rotate' | Down | Drop | Pause | Quit
type tile = L | L' | Z | Z' | O | I | T
type rotation = R0 | R1 | R2 | R3
type stone = Color.t option
type control = Paused of control | Dead | Falling of float | Compacting of float * int list

type configuration = {
  m : int; (* pit dimensions in blocks *)
  n : int;
  p : int; (* block dimensions in pixels *)
  q : int;
  fall_delay : float
}

type state = {
  mutable what : control;
  board : stone array array;
  preview : stone array array;
  mutable score : int;
  mutable lines : int;
  mutable level : int;
  mutable future : rotation * tile;
  mutable present : rotation * tile;
  mutable position : int * int; (* position of present tile, must be consistent with board *)
  mutable delay : float
}

val default_configuration : configuration
val initial_state : configuration -> state

(* configuration -> state -> time since last invoked -> input -> update-display? *)
val update_board: configuration -> state -> float -> input option -> bool
