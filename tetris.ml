(* Tetris *)
(* $Id: tetris.ml,v 1.4 2003/11/18 22:51:09 berke Exp $ *)
(* vim:set ts=4: *)
(* By Oguz Berke DURAK *)

open Human
open Allegro

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

let pixel_dimensions c = (2 * c.m * c.p + 200, c.n * c.q + 200)

(* let fall_delay = 0.1250000 (* s *) *)
let compact_delay = 0.250 (* s *)
let shade_variation = 0.300

type input = Left | Right | Rotate | Rotate' | Down | Drop | Pause | Quit
type tile = L | L' | Z | Z' | O | I | T
type rotation = R0 | R1 | R2 | R3
type stone = Human.Color.t option
type control = Paused of control | Dead | Falling of float | Compacting of float * int list

let clockwise = function R0 -> R1 | R1 -> R2 | R2 -> R3 | R3 -> R0
let counter_clockwise = function R0 -> R3 | R1 -> R0 | R2 -> R1 | R3 -> R2
	
(* A
   BC
 
    C
   AB

   CB
    A

   BA
   C *)

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

let initial_state c =
  {
   what = Falling c.fall_delay;
   board = Array.make_matrix c.m c.n None;
   score = 0;
   lines = 0;
   future = random_tile ();
   present = random_tile ();
   position = (1,c.n/2);
   delay = c.fall_delay }

(* c : configuration *)
(* q : current state *)
(* t : time elapsed since last call to this transition function *)
(* k : key pressed *)

let sf = Printf.sprintf
let debug msg = Printf.eprintf "debug: %s\n" msg; flush stderr

let update_board c q t k =
  let k = match k with
  | Some(Keypad.Pressed Keypad.Left) -> Some(Left)
  | Some(Keypad.Pressed Keypad.Right) -> Some(Right)
  | Some(Keypad.Pressed Keypad.Down) -> Some(Down)
  | Some(Keypad.Pressed Keypad.Up) -> Some(Rotate)
  | Some(Keypad.Pressed Keypad.Start) -> Some(Pause)
  | Some(Keypad.Pressed Keypad.L) -> Some(Down)
  | Some(Keypad.Pressed Keypad.R) -> Some(Down)
  | Some(Keypad.Pressed Keypad.Select) -> Some(Drop)
  | _ -> None
  in
  let reset_game () =
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
  in
  let occupied i j =
	(i < 0 or j < 0 or i >= c.m or j >= c.n or
	 q.board.(i).(j) <> None)
  in
  let might_occupy t i j = List.for_all (fun (di,dj) -> not (occupied (i + di) (j + dj))) (stones_of_tile t) in
  let carve_stone t i j =
	let c = Color.Shade((Random.float (2.0 *. shade_variation)) -. shade_variation, Color.Dark(color_of_tile t)) in
	List.iter (fun (di,dj) -> q.board.(i + di).(j + dj) <- Some c) (stones_of_tile t)
  in
  let next_tile () =
	q.present <- q.future;
	q.future <- random_tile ();
	let (i,j) = (1,c.n/2) in
	q.position <- (i,j);
	if might_occupy q.present i j then
	  begin
		q.what <- Falling(q.delay);
	  end
	else
	  begin
		q.what <- Dead;
	  end
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
		Some Left when might_occupy q.present i (j - 1) -> (i,j - 1)
	  | Some Right when might_occupy q.present i (j + 1) -> (i,j + 1)
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
  let rec check i j = j = c.n or (q.board.(i).(j) <> None && check i (j + 1)) in
  let do_compaction () =
	let rec loop r i =
	  if i = c.m then
		r
	  else
		if check i 0 then
		  loop r (i + 1)
		else
		  loop (i::r) (i + 1)
	in
	let rec loop' i = function
		i'::r ->
		  Array.blit q.board.(i') 0 q.board.(i - 1) 0 c.n;
		  loop' (i - 1) r
	  | [] -> loop'' i
	and loop'' i =
	  if i = 0 then
		()
	  else
		begin
		  Array.fill q.board.(i - 1) 0 c.n None;
		  loop'' (i - 1)
		end
	in
	loop' c.m (loop [] 0);
	next_tile ()
  in
  let reached_bottom () =
    (* piece reached bottom *)
	begin
	  let (i,j) = q.position in
	  carve_stone q.present i j;
	  (* check if any lines are completed *)
	  let rec loop r i =
		if i = c.m then
		  r
		else
		  if check i 0 then
			loop (i::r) (i + 1)
		  else
			loop r (i + 1)
	  in
	  match loop [] 0 with
	  | [] -> next_tile ()
	  | l ->
          let r = List.length l in
          q.lines <- r + q.lines;
          q.score <- r * r + q.score;
		  q.what <- Compacting(compact_delay,l);
	end
  in
  let as_usual t =
	q.what <- Falling(t);
  in
  let attempt_to_rotate_piece ccw =
	let (r,t) = q.present in
	let r' = if ccw then counter_clockwise r else clockwise r in
	let (i,j) = q.position in
	if might_occupy (r',t) i j then
	  q.present <- (r',t)
	else
	  ()
  in
  match (q.what,k) with
  | (Paused c,Some Pause) ->
	  q.what <- c;
  | (Paused _,_) -> ()
  | (_,Some Pause) ->
	  q.what <- Paused q.what;
  | (Falling x,Some Drop) ->
      while not (move_piece 1 k) do
        ()
      done;
      reached_bottom ()
  | (Falling x,_) ->
	  let di = match k with Some Down -> 1 | _ -> 0 in
	  begin
		match k with
		  Some Rotate -> attempt_to_rotate_piece true
		| Some Rotate' -> attempt_to_rotate_piece false
		| _ -> ()
	  end;
	  if t < x then
		if move_piece (max 0 di) k then reached_bottom () else as_usual (x -. t)
	  else
		if move_piece 1 k then reached_bottom () else as_usual q.delay
  | (Compacting(x,l),None) ->
	  if t < x then
		begin
		  q.what <- Compacting(x -. t,l);
		end
	  else
		do_compaction ()
  | (Compacting(_,_),_) -> do_compaction ()
  | (Dead,Some Drop) ->
	  reset_game ();
  | (Dead,_) -> ()

let transition c q t k =
  let k = match k with
  | Some(Keypad.Pressed Keypad.Left) -> Some(Left)
  | Some(Keypad.Pressed Keypad.Right) -> Some(Right)
  | Some(Keypad.Pressed Keypad.Down) -> Some(Down)
  | Some(Keypad.Pressed Keypad.Up) -> Some(Rotate)
  | Some(Keypad.Pressed Keypad.Start) -> Some(Pause)
  | Some(Keypad.Pressed Keypad.L) -> Some(Down)
  | Some(Keypad.Pressed Keypad.R) -> Some(Down)
  | Some(Keypad.Pressed Keypad.Select) -> Some(Drop)
  | _ -> None
  in
  let reset_game () =
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
  in
  let draw_board () =
	match q.what with
	| Paused _ -> [Display.Draw_string(Color.White,c.i0 + (c.m * c.p) / 2,c.j0,"- PAUSED -")]
	| _ ->
		let com = match q.what with Compacting(_,l) -> l | _ -> [] in
		let rec loop_i i r =
		  if i = c.m then
			r
		  else
			loop_j i 0 r (List.mem i com)
		and loop_j i j r hi =
		  if j = c.n then
			loop_i (i + 1) r
		  else
			match q.board.(i).(j) with 
			  None -> loop_j i (j + 1) r hi
			| Some x -> loop_j i (j + 1)
				  (Display.Fill_rectangle((if hi then Color.Mix(x,0.5,Color.White) else Color.Dark x),
										  c.i0 + i * c.p,c.j0 + j * c.q,c.p,c.q)::r)
				  hi
		in
		let l =
		  loop_i 0
			(match q.what with
			| Compacting _ -> []
			| _ ->
				let (i,j) = q.position
				in
				(List.map
				  (fun (u,v) ->
					(Display.Fill_rectangle(color_of_tile q.present,
											c.i0 + (i + u) * c.p,c.j0 + (j + v) * c.q,c.p,c.q)))
				  (stones_of_tile q.present))@
                (List.map
				  (fun (u,v) ->
					(Display.Fill_rectangle(color_of_tile q.future,
											c.f_i0 + u * c.p,c.f_j0 + v * c.q,c.p,c.q)))
				  (stones_of_tile q.future)))
		in
		((Display.Fill_rectangle(
          (* Color.Mix(Color.Black,0.1,(Color.Mix(Color.Blue,0.6,Color.Green))), *)
          Color.Black,
          c.i0,c.j0,c.m * c.p,c.n * c.q))
          ::l)
		@(match q.what with
		| Dead -> l@[Display.Draw_string(Color.Red,c.i0 + (c.m * c.p) / 2,c.j0,"YOU DIED - PRESS SPACE")]
		| _ ->
            [Display.Draw_string(Color.Black,c.s_i0,c.s_j0,
               sf "%d lines, %d pts (%f ms)"
                  q.lines
                  q.score
                  (1000.0 *. q.delay))])
  in
  let occupied i j =
	(i < 0 or j < 0 or i >= c.m or j >= c.n or
	 q.board.(i).(j) <> None)
  in
  let might_occupy t i j = List.for_all (fun (di,dj) -> not (occupied (i + di) (j + dj))) (stones_of_tile t) in
  let carve_stone t i j =
	let c = Color.Shade((Random.float (2.0 *. shade_variation)) -. shade_variation, Color.Dark(color_of_tile t)) in
	List.iter (fun (di,dj) -> q.board.(i + di).(j + dj) <- Some c) (stones_of_tile t)
  in
  let next_tile () =
	q.present <- q.future;
	q.future <- random_tile ();
	let (i,j) = (1,c.n/2) in
	q.position <- (i,j);
	if might_occupy q.present i j then
	  begin
		q.what <- Falling(q.delay);
		(draw_board (), Some(q.delay))
	  end
	else
	  begin
		q.what <- Dead;
		(draw_board (), None)
	  end
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
		Some Left when might_occupy q.present i (j - 1) -> (i,j - 1)
	  | Some Right when might_occupy q.present i (j + 1) -> (i,j + 1)
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
  let rec check i j = j = c.n or (q.board.(i).(j) <> None && check i (j + 1)) in
  let do_compaction () =
	let rec loop r i =
	  if i = c.m then
		r
	  else
		if check i 0 then
		  loop r (i + 1)
		else
		  loop (i::r) (i + 1)
	in
	let rec loop' i = function
		i'::r ->
		  Array.blit q.board.(i') 0 q.board.(i - 1) 0 c.n;
		  loop' (i - 1) r
	  | [] -> loop'' i
	and loop'' i =
	  if i = 0 then
		()
	  else
		begin
		  Array.fill q.board.(i - 1) 0 c.n None;
		  loop'' (i - 1)
		end
	in
	loop' c.m (loop [] 0);
	next_tile ()
  in
  let reached_bottom () =
    (* piece reached bottom *)
	begin
	  let (i,j) = q.position in
	  carve_stone q.present i j;
	  (* check if any lines are completed *)
	  let rec loop r i =
		if i = c.m then
		  r
		else
		  if check i 0 then
			loop (i::r) (i + 1)
		  else
			loop r (i + 1)
	  in
	  match loop [] 0 with
	  | [] -> next_tile ()
	  | l ->
          let r = List.length l in
          q.lines <- r + q.lines;
          q.score <- r * r + q.score;
		  q.what <- Compacting(compact_delay,l);
		  (draw_board (), Some(compact_delay))
	end
  in
  let as_usual t =
	q.what <- Falling(t);
	(draw_board (), Some(t))
  in
  let attempt_to_rotate_piece ccw =
	let (r,t) = q.present in
	let r' = if ccw then counter_clockwise r else clockwise r in
	let (i,j) = q.position in
	if might_occupy (r',t) i j then
	  q.present <- (r',t)
	else
	  ()
  in
  match (q.what,k) with
  | (Paused c,Some Pause) ->
	  q.what <- c;
	  (draw_board (), Some 0.0)
  | (Paused _,_) -> (draw_board (), None)
  | (_,Some Pause) ->
	  q.what <- Paused q.what;
	  (draw_board (), None)
  | (Falling x,Some Drop) ->
      while not (move_piece 1 k) do
        ()
      done;
      reached_bottom ()
  | (Falling x,_) ->
	  let di = match k with Some Down -> 1 | _ -> 0 in
	  begin
		match k with
		  Some Rotate -> attempt_to_rotate_piece true
		| Some Rotate' -> attempt_to_rotate_piece false
		| _ -> ()
	  end;
	  if t < x then
		if move_piece (max 0 di) k then reached_bottom () else as_usual (x -. t)
	  else
		if move_piece 1 k then reached_bottom () else as_usual q.delay
  | (Compacting(x,l),None) ->
	  if t < x then
		begin
		  q.what <- Compacting(x -. t,l);
		  (draw_board (), Some(x -. t))
		end
	  else
		do_compaction ()
  | (Compacting(_,_),_) -> do_compaction ()
  | (Dead,Some Drop) ->
	  reset_game ();
	  (draw_board (), Some q.delay)
  | (Dead,_) -> (draw_board (), None)


let corner x y = 10 + 20 * x, 10 + 20 * y

let coords x y =
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

let () =
  let cfg = default_configuration in
  let (width, height) = pixel_dimensions cfg in
  let q = initial_state cfg in
  allegro_init();
  install_keyboard();
  install_timer();
  init_screen width height;

  set_palette(get_desktop_palette());

  let screen = get_screen() in
  clear_to_color screen (makecol 0 0 0);

  acquire_screen();

  let (screen_w, screen_h) = get_screen_width(), get_screen_height() in

  for i = 0 to cfg.m do
    for j = 0 to cfg.n do
      let col = (makecol 100 100 100) in
      let x1, y1, x2, y2 = coords i j in
      rectfill screen x1 y1 x2 y2 col
    done
  done;

  (* hello world *)
  textout_centre_ex screen (get_font()) "Hello, world!" (screen_w/2) (screen_h/2) (makecol 100 100 100) (color_index(-1));

  release_screen();

  ignore(readkey());
