(* Play *)
(* $Id: play.ml,v 1.4 2003/11/18 22:51:09 berke Exp $ *)
(* vim:set ts=4: *)
(* By Oguz Berke DURAK *)

open Graphics
open Unix
open Human

type notification =
| Graphics_status of status
| Tick

let sf = Printf.sprintf

module Play(G:Game.S) =
  struct
    let ticker pfdo ich och =
      let buf = String.make 1 ' ' in
      while true do
        let d = Event.sync (Event.receive ich) in
        try
          ignore (ThreadUnix.timed_read pfdo buf 0 1 d);
          Event.sync (Event.send och Tick)
        with
        | Unix_error(ETIMEDOUT,_,_) ->
          Event.sync (Event.send och Tick)
          (* try
            ignore (read pfdo buf 0 1)
          with
          | x ->
              Printf.printf "%s!!\n" (Printexc.to_string x);
              flush Pervasives.stdout *)
      done

    let key_listener ch =
      let rec loop () =
        let ev = wait_next_event [Key_pressed] in
        ignore (Event.sync (Event.send ch (Graphics_status ev)));
        loop ()
      in
      loop ()

    let start () =
      Random.self_init ();
      let (pfdo,pfdi) = Unix.pipe () in
      let notification_channel = Event.new_channel ()
      and chronorequest_channel = Event.new_channel ()
      and chronoreply_channel = Event.new_channel ()
      in
      let t1 = Thread.create (ticker pfdo chronorequest_channel) chronoreply_channel
      and t2 = Thread.create key_listener notification_channel
      in
      let ack () =
        ignore (write pfdi "." 0 1)
      in
      let wait_next_event_with_timeout t l =
        Event.sync (Event.send chronorequest_channel t);
        match Event.sync (Event.choose [Event.receive notification_channel;
                                        Event.receive chronoreply_channel]) with
        | Tick -> None
        | Graphics_status st ->
            ack ();
            ignore (Event.sync (Event.receive chronoreply_channel));
            Some st
      in
      let cfg = G.default_configuration in
      let (m,n) = G.pixel_dimensions cfg in
      open_graph (sf " %dx%d" n m);
      auto_synchronize false;
      set_font "-*-helvetica-bold-r-normal-*-18-*-*-*-*-*-*-*";
      let q = G.initial_state cfg in
      let col x =
        let f y = max 0 (min 255 (int_of_float (255.0 *. y))) in
        let (r,g,b) = Color.rgb_of_color x in
        rgb (f r) (f g) (f b)
      in
      let do_draw drw =
        clear_graph ();
        List.iter (function
          | Display.Fill_rectangle(c,i,j,m',n') ->
              set_color (col c);
              fill_rect j (m - i - m') n' m'
          | Display.Draw_string(c,i,j,x) ->
              set_color (col c);
              moveto j (m - i);
              draw_string x) drw;
        synchronize ()
      in
      let rec loop alarm last_t last_drw =
        let now = gettimeofday () in
        let key =
          match
            (*if alarm < 0.0 or (alarm > 0.0 && now >= alarm) then
              Some(wait_next_event [Key_pressed; Poll]) (* Poll *)
            else
              if alarm = 0.0 then
                Some(wait_next_event [Key_pressed])
              else *)
                wait_next_event_with_timeout
                  (if alarm < 0.0 or (alarm > 0.0 && now >= alarm) then 0.01
                  else alarm -. now)
                [Key_pressed]
          with
          | Some(st) ->
            if st.keypressed then
              match st.key with
              | 'h' -> Some(Keypad.Pressed(Keypad.Left))
              | 'l' -> Some(Keypad.Pressed(Keypad.Right))
              | 'j' -> Some(Keypad.Pressed(Keypad.Down))
              | 'k' -> Some(Keypad.Pressed(Keypad.Up))
              | 'a' -> Some(Keypad.Pressed(Keypad.A))
              | 'z' -> Some(Keypad.Pressed(Keypad.B))
              | 's' -> Some(Keypad.Pressed(Keypad.L))
              | 'x' -> Some(Keypad.Pressed(Keypad.R))
              | '\t'|' ' -> Some(Keypad.Pressed(Keypad.Select))
              | '`' -> Some(Keypad.Pressed(Keypad.Start))
              | _ -> None
            else
              None
          | None -> None
        in
        let now = gettimeofday () in
        let (drw,tim) =
          G.transition cfg q
            (if last_t > 0.0 then now -. last_t else 0.0)
            key
        in
        loop
          (match tim with None -> 0.0 | Some t -> now +. t)
          now
          (if drw <> last_drw then
            begin
              do_draw drw;
              drw
            end
          else
            drw)
      in
      loop (-1.0) 0.0 [];
      close_graph ()
    end


let _ =
  let module T = Play(Tetris) in
  T.start ()
