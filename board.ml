open Core.Std
open Types
open Sprite

type t = {
  rows : int;
  cols : int;
  grid : stone array array
}

let make r c = {
  rows = r;
  cols = c;
  grid = Array.make_matrix ~dimx:r ~dimy:c None
}

let not_none x = x <> None

(* Select all indices for which the predicate is true *)
let select_rows board p = 
  Array.foldi board.grid ~init:[] ~f:(fun i a e -> if p e then i :: a else a) 

let filled row = Array.for_all row not_none

let filled_rows board = select_rows board filled 

let clear_row board r = Array.fill board.grid.(r) 0 board.cols None

let clear board =
  for r = 0 to board.rows - 1 do
    clear_row board r
  done

let in_board board r c =
  r >= 0 && r < board.rows && c >= 0 && c < board.cols

(* have an implicit border of occupied cells around the board *)
let occupied board r c =
  not (in_board board r c) || not_none board.grid.(r).(c)

let copy_row board r1 r2 =
  let grid = board.grid in
  Array.blit grid.(r1) 0 grid.(r2) 0 board.cols

let can_place_piece board sprite =
  List.for_all (Sprite.coords sprite) (fun (r, c) ->
    not (occupied board r c))

(* caller is responsible for checking can_place_piece first *)
let draw_piece board sprite color =
  List.iter (Sprite.coords sprite) (fun (r, c) ->
    board.grid.(r).(c) <- color)

let place_piece board sprite =
  let color = Piece.color sprite.piece in
  draw_piece board sprite (Some color)

let remove_piece board sprite =
  draw_piece board sprite None

(* remove all filled rows, compact the remainder *)
let compact board =
  let unfilled x = not (filled x) in
  let keep_rows = select_rows board unfilled in
  let rec compact i r = match r with
    | i'::r' -> copy_row board i' (i - 1);
      compact (i - 1) r'
    | [] -> for j = (i - 1) downto 0 do
        clear_row board j
      done
  in
  compact board.rows keep_rows

(* provide a gradual fading effect for filled-in rows *)
let shade_filled_rows board =
  let open Color in
  let grey = Dark(White) in
  let darken = function 
    | None -> None 
    | Some c -> Some(Mix(c, 0.7, grey))
  in
  let darken_row r =
    let row = board.grid.(r) in
    Array.iteri row ~f:(fun i e -> row.(i) <- darken e)
  in
  List.iter (select_rows board filled) darken_row
