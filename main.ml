open Color
open Tetris
open Allegro

let pixel_dimensions c = (c.m * c.p, c.n * c.q)

(* graphics *)
let cell_boundaries c x y =
  let x1, y1 = 10 + c.q * x, 10 + c.p * y in
  x1+1, y1+1, x1+c.q-1, y1+c.p-1

let preview_cell_boundaries c x y = cell_boundaries c (x+c.n+2) (y+10)

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

let initbg bgfile =
  let pal = get_desktop_palette () in
  try load_bitmap bgfile pal
  with _ ->
    set_gfx_mode GFX_TEXT 0 0 0 0;
    allegro_message("Error reading "^ bgfile ^"!\n");
    exit 1

let allegro_color c =
  let (r, g, b) = Color.rgb_of_color c in
  let s x = int_of_float (255.0 *. x) in
  makecol (s r) (s g) (s b)
;;

let display_board screen bg w h c q =
  acquire_screen();

  blit bg screen 0 0 0 0 w h;

  for i = 0 to (c.m - 1) do
    for j = 0 to (c.n - 1) do
      let s = q.board.(i).(j) in
      let col = match s with
      | None -> (makeacol 150 50 50 200)
      | Some c -> allegro_color c
      in
      drawing_mode (match s with None -> DRAW_MODE_TRANS | _ -> DRAW_MODE_SOLID);
      let x1, y1, x2, y2 = cell_boundaries c j i in
      rectfill screen x1 y1 x2 y2 col
    done
  done;

  for i = 0 to c.ph - 1 do
    for j = 0 to c.pw - 1 do
      let s = q.preview.(i).(j) in
      let col = match s with
      | None -> (makeacol 0 0 0 255)
      | Some c -> allegro_color c
      in
      drawing_mode DRAW_MODE_SOLID;
      let x1, y1, x2, y2 = preview_cell_boundaries c j i in
      rectfill screen x1 y1 x2 y2 col
    done
  done;

  let (h, w) = pixel_dimensions c in
  let (ty, tx) = (50, w + 50) in
  let font = get_font() in
  let fg, bg = (allegro_color Color.White), (allegro_color Color.Black) in
  let bps = floor (((1.0 /.  q.delay) +. 0.5)) in
  drawing_mode DRAW_MODE_SOLID;
  rectfill screen (tx - 10) (ty - 10) (tx+90) (ty+110) bg;
  textprintf_ex screen font tx ty fg bg "Score: %d" q.score;
  textprintf_ex screen font tx (ty + 32) fg bg "Lines: %d" q.lines;
  textprintf_ex screen font tx (ty + 64) fg bg "Level: %d" q.level;
  textprintf_ex screen font tx (ty + 92) fg bg "Speed: %.0f" bps;
  release_screen()
;;

let () =
  let cfg = default_configuration in
  let (h, w) = pixel_dimensions cfg in
  let (height, width) = (h + 50, w + 200) in

  let q = initial_state cfg in
  Random.self_init ();

  allegro_init();
  install_keyboard();
  install_timer();
  set_color_depth 32;
  set_color_conversion COLORCONV_TOTAL;
  init_screen width height;

  set_palette(get_desktop_palette());
  set_trans_blender 0 0 0 127;

  let bg = initbg "bg.bmp" in

  let screen = get_screen() in
  clear_to_color screen (makecol 0 0 0);

  let quit = ref false in
  let c = ref 0 in
  let get_timer () =
    let t = retrace_count() - !c in
    (float_of_int t) /. 70.0
  in
  let update k =
    let tau = get_timer() in
    if update_board cfg q tau k then
      display_board screen bg width height cfg q;
    c := retrace_count()
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
      update k;
      if k = Some(Quit) then quit := true;
    done;
    let tau = get_timer() in
    if tau > 0.05 then
      update None;
    rest 10
  done;
