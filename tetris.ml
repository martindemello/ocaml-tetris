(*pp $PP *)
(* An implementation of tetris using allegro-ocaml *)
(* By Martin DeMello <martindemello@gmail.com> *)
(* based on mltetris by Oguz Berke DURAK *)

open Printf
include Types
include World

(* w : current world *)
(* t : time elapsed since last call to this transition function *)
(* action : key press action *)
let update w t action =
  let compacting x t =
    if t >= x then
      World.finish_compacting w
    else begin
      w.state <- Compacting(x -. t);
      Board.shade_filled_rows w.board
    end
  in
  let reached_bottom () =
    World.finalize_current_piece w
  in
  let keep_falling t =
    w.state <- Falling(t)
  in
  match (w.state, action) with
  | (Paused c, Some Pause) -> World.unpause w
  | (Paused _, _) -> w.dirty <- false
  | (_, Some Pause) -> World.pause w
  | (Compacting(x), None) -> compacting x t
  | (Compacting(_), _) -> compacting t t
  | (Dead, Some Drop) -> World.reset w 0
  | (Dead, _) -> w.dirty <- false
  | (Falling x, Some Drop) -> World.drop_current_piece w
  | (Falling x, _) ->
      let open Sprite in
      World.remove_current_piece w;
      let s = w.current in
      let time_up = t >= x in
      let drop = (time_up || action = Some Down) in
      let t' = if time_up then w.delay else x -. t in
      if World.move_current_piece w ~drop ~action then begin
        World.place_current_piece w;
        keep_falling t'
      end else
        reached_bottom ();
      w.dirty <- (w.state = Dead) ||
                 (s.row, s.col) <> (w.current.row, w.current.col)
