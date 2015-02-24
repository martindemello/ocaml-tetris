open Core.Std
open Types
open Sprite
open Board

type t = {
  board : Board.t;
  preview : Board.t;
  mutable dirty : bool;
  mutable current : Sprite.t;
  mutable state : state;
  mutable score : int;
  mutable lines : int;
  mutable level : int;
  mutable future : piece;
  mutable delay : float;
  (* starting position for new pieces *)
  start_row : int;
  start_col : int;
  conf : configuration
}

let update_preview w =
  Board.clear w.preview;
  Board.place_piece w.preview { Sprite.
    piece = w.future;
    rotation = R0;
    row = Piece.preview_offset w.future;
    col = 1
  }

let level_delay c level =
  (0.9 ** (Float.of_int level)) *. c.fall_delay

let make conf level =
  let mid = conf.n / 2 in
  let w = {
    board = Board.make conf.m conf.n;
    preview = Board.make conf.ph conf.pw;
    dirty = true; (* render the initial board *)
    state = Falling conf.fall_delay;
    score = 0;
    lines = 0;
    level = level;
    future = Piece.random ();
    current = Sprite.make_random 1 mid;
    delay = level_delay conf level;
    start_row = 1;
    start_col = mid;
    conf = conf
  }
  in
  update_preview w;
  w

let reset w level =
  Board.clear w.board;
  w.state <- Falling w.conf.fall_delay;
  w.dirty <- true;
  w.score <- 0;
  w.lines <- 0;
  w.level <- 0;
  w.delay <- w.conf.fall_delay;
  w.future <- Piece.random ();
  w.current <- Sprite.make_random w.start_row w.start_col;
  update_preview w

(* Place piece on board *)

let place_current_piece w =
  Board.place_piece w.board w.current

let remove_current_piece w =
  Board.remove_piece w.board w.current

let fix_current_piece w =
  let s = w.current in
  let c = Color.tweak (Piece.color s.piece) in
  Board.draw_piece w.board s (Some c)

let can_place w s =
  Board.can_place_piece w.board s

(* State transitions *)

(* Pause / Unpause *)

let pause w =
  match w.state with
  | Paused _ -> ()
  | s -> begin
      w.state <- Paused s;
      w.dirty <- true
    end

let unpause w =
  match w.state with
  | Paused s -> begin
      w.state <- s;
      w.dirty <- true
    end
  | _ -> ()

(* Next piece *)

let next_piece w =
  w.current <- Sprite.make w.future w.start_row w.start_col;
  w.future <- Piece.random ();
  update_preview w;
  w.dirty <- true;
  if can_place w w.current then
    w.state <- Falling(w.delay)
  else
    w.state <- Dead

let clear_lines w n =
  let compact_delay = 0.250 (* s *) in
  w.state <- Compacting(compact_delay);
  w.lines <- n + w.lines;
  w.score <- n * n + w.score;
  w.dirty <- true;
  if w.lines > 10 + w.level * 10 then
    begin
      w.level <- w.level + 1;
      w.delay <- level_delay w.conf w.level;
    end

let finalize_current_piece w =
  fix_current_piece w;
  w.score <- 1 + w.score;
  w.dirty <- true;
  match List.length (Board.filled_rows w.board) with
  | 0 -> next_piece w
  | n -> clear_lines w n

let finish_compacting w =
  Board.compact w.board;
  next_piece w

(* Move piece on board *)
let move_current_piece w ~drop ~action =
  let d = if drop then 1 else 0 in
  let s' = Sprite.move_down w.current d in
  let f = match action with
  | Some Left -> Sprite.move_left
  | Some Right -> Sprite.move_right
  | Some Rotate -> Sprite.rotate_cw
  | Some Rotate' -> Sprite.rotate_ccw
  | _ -> Sprite.dont_move
  in
  let s'' = f s' in
  let s = if can_place w s'' then s'' else s' in
  if can_place w s then
    ( w.current <- s; true )
  else
    false

let drop_current_piece w =
  let action = Some Drop in
  remove_current_piece w;
  while (move_current_piece w ~drop:true ~action) do
    ()
  done;
  place_current_piece w;
  finalize_current_piece w
