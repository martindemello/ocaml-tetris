open Types
open Core.Std

let trans (dx, dy) l = List.map l (fun (x, y) -> (x + dx, y + dy))

let base_offsets = function
| Z  -> trans (-1,-1) [0,0;0,1;1,1;1,2]
| Z' -> trans (-1,-1) [0,1;0,2;1,0;1,1]
| T  -> trans ( 0, 0) [-1,0;0,-1;0,0;0,1]
| O  -> trans ( 0, 0) [0,0;0,1;1,1;1,0]
| L  -> trans ( 0, 0) [-1,0;0,0;1,0;1,1]
| L' -> trans ( 0, 0) [-1,0;0,0;1,0;1,-1]
| I  -> trans (-1, 0) [0,0;1,0;2,0;3,0]

let rotate_to = function
| R0 -> fun (i,j) -> (i, j)
| R1 -> fun (i,j) -> (j,-i)
| R2 -> fun (i,j) -> (-i,-j)
| R3 -> fun (i,j) -> (-j,i)

let offsets (rot, t) =
  let s = base_offsets t in 
  match (rot, t) with
  | _, O -> s
  | (R0 | R2), (Z | Z' | I) -> s
  | _, _ -> List.map s (rotate_to rot)

let color t =
  match t with
  | L -> Color.Cyan
  | L' -> Color.Magenta
  | Z -> Color.Red
  | Z' -> Color.Green
  | O -> Color.Blue
  | I -> Color.White
  | T -> Color.Yellow

let preview_offset piece = match piece with
   I -> 1 | O -> 1 | _ -> 2

let random () =
  match Random.int 7 with
  0 -> L | 1 -> L' | 2 -> Z | 3 -> Z' | 4 -> O  | 5 -> I | _ -> T
