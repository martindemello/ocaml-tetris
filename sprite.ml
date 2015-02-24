open Core.Std
open Types

(* All the state held by an actively falling piece *)
type t = {
  piece : piece;
  rotation : rotation;
  row : int;
  col : int
}

let offsets s = Piece.offsets (s.rotation, s.piece);;

let coords s = List.map (offsets s) (fun (dr, dc) ->
  (s.row + dr, s.col + dc))

let make p r c = {
  piece = p;
  rotation = R0;
  row = r;
  col = c
}

let make_random r c = make (Piece.random ()) r c

let move_left s = { s with col = s.col - 1 }
let move_right s = { s with col = s.col + 1 }
let move_down s d = { s with row = s.row + d }

let cw = function R0 -> R1 | R1 -> R2 | R2 -> R3 | R3 -> R0
let ccw = function R0 -> R3 | R1 -> R0 | R2 -> R1 | R3 -> R2

let rotate_cw s = { s with rotation = cw s.rotation }
let rotate_ccw s = { s with rotation = ccw s.rotation }

let dont_move s = s
