(*pp $PP *)
(* Tetris *)
(* $Id: tetris.ml,v 1.4 2003/11/18 22:51:09 berke Exp $ *)
(* vim:set ts=4: *)
(* By Oguz Berke DURAK *)

open Human
open Allegro
open Printf
open Batteries

type vector = int * int
type rectangle = int * int * int * int

type configuration = {
  m : int;
  n : int;
  p : int;
  q : int;
  i0 : int;
  j0 : int;
  f_i0 : int;
  f_j0 : int;
  s_i0 : int;
  s_j0 : int;
  fall_delay : float
}

let default_configuration = {
  m = 20;
  n = 10;
  p = 20;
  q = 20;
  i0 = 0;
  j0 = 0;
  f_i0 = 50;
  f_j0 = 300;
  s_i0 = 100;
  s_j0 = 300;
  fall_delay = 0.200 (* s *)
}

let pixel_dimensions c = (c.m * c.p + 50, c.n * c.q + 200)

(* let fall_delay = 0.1250000 (* s *) *)
let compact_delay = 0.250 (* s *)
let shade_variation = 0.100

type input = Left | Right | Rotate | Rotate' | Down | Drop | Pause | Quit
type tile = L | L' | Z | Z' | O | I | T
type rotation = R0 | R1 | R2 | R3
type stone = Human.Color.t option
type control = Paused of control | Dead | Falling of float | Compacting of float * int list

let clockwise = function R0 -> R1 | R1 -> R2 | R2 -> R3 | R3 -> R0
let counter_clockwise = function R0 -> R3 | R1 -> R0 | R2 -> R1 | R3 -> R2

(* A     C    CB    BA
   BC   AB     A    C   *)

(*
  | Z  -> [0,0;0,1;1,1;1,2]
  | Z' -> [0,0;0,1;1,1;1,0]
  | T  -> [0,1;1,0;1,1;1,2]
  | O  -> [0,0;0,1;1,1;1,0]
  | L  -> [0,0;1,0;2,0;2,1]
  | L' -> [0,0;1,0;2,0;2,-1]
  | I  -> [0,0;1,0;2,0;3,0] *)

let trans (dx,dy) l = List.map (fun (x,y) -> (x+dx,y+dy)) l

let base_stones = function
| Z  -> trans (-1,-1) [0,0;0,1;1,1;1,2]
| Z' -> trans (-1,-1) [0,1;0,2;1,0;1,1]
| T  -> [-1,0;0,-1;0,0;0,1]
| O  -> [0,0;0,1;1,1;1,0]
| L  -> [-1,0;0,0;1,0;1,1]
| L' -> [-1,0;0,0;1,0;1,-1]
| I  -> [0,0;1,0;2,0;3,0]

let map_of_rotation = function
| R0 -> fun x -> x
| R1 -> fun (i,j) -> (j,-i)
| R2 -> fun (i,j) -> (-i,-j)
| R3 -> fun (i,j) -> (-j,i)

let stones_of_tile (rot,t) = match (rot,t) with
| _,O -> base_stones O
| (R0|R2),(Z|Z'|I) -> base_stones t
| (R1|R3),(Z|Z'|I) -> List.map (map_of_rotation R1) (base_stones t)
| _,_ -> List.map (map_of_rotation rot) (base_stones t)

let color_of_tile (_,t) =
  match t with
  | L -> Color.Cyan
  | L' -> Color.Magenta
  | Z -> Color.Red
  | Z' -> Color.Green
  | O -> Color.Blue
  | I -> Color.White
  | T -> Color.Yellow

let height_of_tile = function
  Z -> 2 | Z' -> 2 | T -> 2 | O -> 2 | L -> 3 | L' -> 3 | I -> 3

type state = {
  mutable what : control;
  board : stone array array;
  mutable score : int;
  mutable lines : int;
  mutable future : rotation * tile;
  mutable present : rotation * tile;
  mutable position : int * int; (* position of present tile, must be consistent with board *)
  mutable delay : float
}

let random_tile () =
  R0,
  match Random.int 7 with
  0 -> L | 1 -> L' | 2 -> Z | 3 -> Z' | 4 -> O  | 5 -> I | _ -> T

let initial_state c = {
  what = Falling c.fall_delay;
  board = Array.make_matrix c.m c.n None;
  score = 0;
  lines = 0;
  future = random_tile ();
  present = random_tile ();
  position = (1,c.n/2);
  delay = c.fall_delay
}

(* c : configuration *)
(* q : current state *)
(* t : time elapsed since last call to this transition function *)
(* k : key pressed *)

let sf = Printf.sprintf
let debug msg = Printf.eprintf "debug: %s\n" msg; flush stderr

let reset_board c q =
  q.what <- Falling c.fall_delay;
  for i = 0 to c.m - 1 do
    for j = 0 to c.n - 1 do
      q.board.(i).(j) <- None
    done
  done;
  q.score <- 0;
  q.lines <- 0;
  q.future <- random_tile ();
  q.present <- random_tile ();
  q.position <- (1,c.n/2)

let filled c q i =
  Enum.for_all (fun (j) -> q.board.(i).(j) <> None) (0 -- (c.n - 1))

let compact_board c q =
  let keep_rows = [? List: i | i <- (c.m - 1) --- 0 ; not(filled c q i) ?] in
  let fill_empty i = Array.fill q.board.(i - 1) 0 c.n None in
  let rec compact i = function
    | i'::r ->
        Array.blit q.board.(i') 0 q.board.(i - 1) 0 c.n;
        compact (i - 1) r
    | [] -> Enum.iter fill_empty (i --- 1)
  in
  compact c.m keep_rows

let shade_compacting c q =
  let grey = Color.Dark(Color.White) in
  let filled = [? List: i | i <- (c.m - 1) --- 0 ; filled c q i ?] in
  let darken = function None -> None | Some c -> Some(Color.Mix(c, 0.7, grey)) in
  let darken_line i =
    Enum.iter (fun j -> q.board.(i).(j) <- darken q.board.(i).(j)) (0 -- (c.n - 1))
  in
  List.iter darken_line filled

(* vary the colour of a tile slightly when it's done falling *)
let varied_color tile =
  Color.Shade(
    (Random.float (2.0 *. shade_variation)) -. shade_variation,
    Color.Dark(color_of_tile tile))

let update_board c q t k =
  let occupied i j =
    (i < 0 or j < 0 or i >= c.m or j >= c.n or q.board.(i).(j) <> None)
  in
  let might_occupy t i j = List.for_all (fun (di,dj) -> not (occupied (i + di) (j + dj))) (stones_of_tile t) in
  let blit_stone t i j c =
    List.iter (fun (di,dj) -> q.board.(i + di).(j + dj) <- c) (stones_of_tile t)
  in
  let carve_stone t i j =
    let c = varied_color t in
    blit_stone t i j (Some c)
  in
  let place t =
    let (i,j) = q.position in
    let c = color_of_tile t in
    blit_stone t i j (Some c)
  in
  let remove t =
    let (i,j) = q.position in
    blit_stone t i j None
  in
  let next_tile () =
    q.present <- q.future;
    q.future <- random_tile ();
    let (i,j) = (1,c.n/2) in
    q.position <- (i,j);
    if might_occupy q.present i j then
      q.what <- Falling(q.delay)
    else
      q.what <- Dead
  in
  (* move the piece one position down *)
  (* returns true iff no restriction on horizontal motion could give a valid position, i.e. bottom was reached *)
  let move_piece di k =
    (* piece falls *)
    (* decrease by one *)
    let (i,j) = q.position in
    let i = i + di in
    let (i,j) =
      match k with
      | Some Left  when might_occupy q.present i (j - 1) -> (i, j - 1)
      | Some Right when might_occupy q.present i (j + 1) -> (i, j + 1)
      | _ -> (i,j)
    in
    if might_occupy q.present i j then
      begin
        q.position <- (i,j);
        false
      end
    else
      true
  in
  let do_compaction () =
    compact_board c q;
    next_tile ()
  in
  let reached_bottom () =
    (* piece reached bottom *)
    begin
      let (i,j) = q.position in
      carve_stone q.present i j;
      (* check if any lines are completed *)
      let filled_rows = [? List: i | i <- (c.m - 1) --- 0 ; filled c q i ?] in
      match filled_rows with
      | [] -> next_tile ()
      | l ->
          let r = List.length l in
          q.lines <- r + q.lines;
          q.score <- r * r + q.score;
          q.what <- Compacting(compact_delay,l);
    end
  in
  let as_usual t = q.what <- Falling(t) in
  let attempt_to_rotate_piece ccw =
    let (r,t) = q.present in
    let r' = if ccw then counter_clockwise r else clockwise r in
    let (i,j) = q.position in
    if might_occupy (r',t) i j then
      begin
        q.present <- (r',t);
        true
      end
    else
      false
  in
  match (q.what,k) with
  | (Paused c,Some Pause) -> q.what <- c; true
  | (Paused _,_) -> false
  | (_,Some Pause) -> q.what <- Paused q.what; true
  | (Falling x,Some Drop) ->
      remove q.present;
      while not (move_piece 1 k) do
        ()
      done;
      place q.present;
      reached_bottom ();
      true
  | (Falling x,_) ->
      let flag = ref false in
      remove q.present;
      let di = match k with Some Down -> 1 | _ -> 0 in
      begin
        match k with
        | Some Rotate  -> flag := attempt_to_rotate_piece true
        | Some Rotate' -> flag := attempt_to_rotate_piece false
        | _ -> ()
      end;
      let oldi, oldj = q.position in
      if t < x then
        if move_piece di k then reached_bottom () else as_usual (x -. t)
      else
        if move_piece 1 k then reached_bottom () else as_usual q.delay;
      if (oldi, oldj) <> q.position then flag := true;
      place q.present;
      !flag
  | (Compacting(x,l),None) ->
    if t < x then
      begin
        q.what <- Compacting(x -. t,l);
        shade_compacting c q;
        true
      end
    else
      begin
        do_compaction (); true
      end
  | (Compacting(_,_),_) -> do_compaction (); true
  | (Dead,Some Drop) -> reset_board c q; true
  | (Dead,_) -> false

(* graphics *)
let corner x y = 10 + 20 * y, 10 + 20 * x

let cell_boundaries x y =
  let x1, y1 = corner x y in
  x1+1, y1+1, x1+19, y1+19

let init_screen width height =
  begin
    try set_gfx_mode GFX_AUTODETECT_WINDOWED width height 0 0;
    with _ ->
      try set_gfx_mode GFX_SAFE width height 0 0;
      with _ ->
        set_gfx_mode GFX_TEXT 0 0 0 0;
        allegro_message("Unable to set any graphic mode\n"^ (get_allegro_error()) ^"\n");
        exit 1;
  end


let allegro_color c =
  let (r, g, b) = Color.rgb_of_color c in
  let s x = int_of_float (255.0 *. x) in
  makecol (s r) (s g) (s b)
;;

let display_board screen cfg q =
  acquire_screen();

  for i = 0 to (cfg.m - 1) do
    for j = 0 to (cfg.n - 1) do
      let col = match q.board.(i).(j) with
      | None -> (makecol 100 100 100)
      | Some c -> allegro_color c
      in
      let x1, y1, x2, y2 = cell_boundaries i j in
      rectfill screen x1 y1 x2 y2 col
    done
  done;

  release_screen()
;;

let () =
  let cfg = default_configuration in
  let (height, width) = pixel_dimensions cfg in
  let q = initial_state cfg in
  Random.init(1000);

  allegro_init();
  install_keyboard();
  install_timer();
  init_screen width height;

  set_palette(get_desktop_palette());

  let screen = get_screen() in
  clear_to_color screen (makecol 0 0 0);

  let quit = ref false in
  let c = ref 0 in
  let get_timer () =
    let t = retrace_count() - !c in
    (float_of_int t) /. 70.0
  in
  while not(!quit) do
    while keypressed() do
      let k = match readkey_scancode() with
      | KEY_UP    -> Some(Rotate)
      | KEY_DOWN  -> Some(Down)
      | KEY_LEFT  -> Some(Left)
      | KEY_RIGHT -> Some(Right)
      | KEY_ESC   -> Some(Quit)
      | KEY_SPACE -> Some(Drop)
      | KEY_P     -> Some(Pause)
      | _         -> None
      in
      let tau = get_timer() in
      if update_board cfg q tau k then
        display_board screen cfg q;
      c := retrace_count();
      if tau > cfg.fall_delay then c := retrace_count();
      if k = Some(Quit) then quit := true;
    done;
    let tau = get_timer() in
    if tau > 0.1 then
      begin
        if update_board cfg q tau None then
          display_board screen cfg q;
        c := retrace_count();
      end
  done;
