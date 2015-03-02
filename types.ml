type piece = L | L' | Z | Z' | O | I | T
type rotation = R0 | R1 | R2 | R3
type stone = Color.t option
type action = Left | Right | Rotate | Rotate' | Down | Drop | Pause | Quit
type state = Paused of state | Dead | Falling of float | Compacting of float

type configuration = {
  m : int; (* pit dimensions in blocks *)
  n : int;
  p : int; (* block dimensions in pixels *)
  q : int;
  ph: int; (* preview dimensions in blocks *)
  pw: int;
  fall_delay : float
}

let default_configuration = {
  m = 20;
  n = 10;
  p = 20;
  q = 20;
  ph = 4;
  pw = 3;
  fall_delay = 0.800 (* s *)
}
