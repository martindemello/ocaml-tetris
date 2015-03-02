open OcsfmlSystem
open OcsfmlWindow
open OcsfmlGraphics
open OcsfmlAudio
open Types
open Bigarray

let ( $ ) f x = f x

module C = struct
  let size = 20.0
end

let font = new font (`File "resources/sansation.ttf")

let pause_message = new text ~font
  ~character_size:40
  ~position:(170.0, 150.0)
  ~color:Color.white
  ~string:"Paused: Press p to unpause"
  ~style:[Bold]
  ()

let dead_message = new text ~font
  ~character_size:40
  ~position:(170.0, 150.0)
  ~color:Color.white
  ~string:"Dead: Press space to start the game"
  ~style:[Bold]
  ()

let top_left row col =
  (C.size *. (float_of_int col), C.size *. (float_of_int row))

let rect_of row col =
  let position = top_left row col in
  let size = (C.size, C.size) in
  new rectangle_shape ~size ~position
    ~outline_thickness:1.
    ~outline_color:Color.white
    ~fill_color:(Color.rgb 100 100 200)
    ()

let draw_board (display: render_window) w =
  let open World in
  let open Board in
  let color = OcsfmlGraphics.Color.rgb 0 0 0 in
  display#clear ~color ();
  let b = w.board in
  for r = 0 to b.rows - 1 do
    for c = 0 to b.cols - 1 do
      match b.grid.(r).(c) with
      | Some _ -> begin
          let rect = rect_of r c in
          display#draw rect
        end
      | None -> ()
    done
  done;
  match w.state with
  | Paused _ -> display#draw pause_message
  | Dead -> display#draw dead_message
  | _ -> ()


let test_tetris () =
  let open World in
  Random.self_init () ;

  let game_width = 600 in
  let game_height = 800 in

  let vm = VideoMode.create ~w:game_width ~h:game_height () in
  let app = new render_window vm "Ocsfml - Tetris" in

  let global_clock = new clock in
  let world = World.make default_configuration 0 in

  let update action =
    let t = Time.as_seconds global_clock#restart in
    Tetris.update world t action;
  in
  let rec event_loop () =
    match app#poll_event with
    | Some e ->
      let open Event in
      begin
        match e with
        | KeyPressed { code = KeyCode.Escape }
        | Closed -> app#close 
        | KeyPressed { code = code ; _ }  ->
          begin
            let action = match code with
              | KeyCode.Up     -> Some(Rotate)
              | KeyCode.Down   -> Some(Down)
              | KeyCode.Left   -> Some(Left)
              | KeyCode.Right  -> Some(Right)
              | KeyCode.Space  -> Some(Drop)
              | KeyCode.P      -> Some(Pause)
              | _              -> None
            in
            update action
          end
        | _ -> () 
      end ;
      event_loop ()
    | None -> () 
  in

  let draw w =
    if w.dirty then draw_board app w
  in

  let timed_update () =
    let tau = Time.as_milliseconds global_clock#get_elapsed_time in
    if (tau > 200) then update None
  in

  let rec main_loop () =
    if app#is_open 
    then 
      begin
        event_loop ();
        timed_update ();
        draw world;
        app#display;
        main_loop ()
      end
  in

  main_loop ();

  Gc.full_major () ;
  font#destroy 

let _ = test_tetris ()
