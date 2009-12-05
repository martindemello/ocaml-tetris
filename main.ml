open Color
open Tetris
open Allegro

let pixel_dimensions c = (c.m * c.p, c.n * c.q)

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

let display_board screen c q =
  acquire_screen();

  for i = 0 to (c.m - 1) do
    for j = 0 to (c.n - 1) do
      let col = match q.board.(i).(j) with
      | None -> (makecol 100 100 100)
      | Some c -> allegro_color c
      in
      let x1, y1, x2, y2 = cell_boundaries i j in
      rectfill screen x1 y1 x2 y2 col
    done
  done;

  let (h, w) = pixel_dimensions c in
  let (ty, tx) = (50, w + 50) in
  let font = get_font() in
  let fg, bg = (allegro_color Color.White), (allegro_color Color.Black) in
  rectfill screen tx ty (tx+100) (ty+64) bg;
  textprintf_ex screen font tx ty fg bg "Score: %d" q.score;
  textprintf_ex screen font tx (ty + 32) fg bg "Lines: %d" q.lines;
  textprintf_ex screen font tx (ty + 64) fg bg "Level: %d" q.level;
  release_screen()
;;

let () =
  let cfg = default_configuration in
  let (h, w) = pixel_dimensions cfg in
  let (height, width) = (h + 50, w + 200) in

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
      end;
    rest 10
  done;

