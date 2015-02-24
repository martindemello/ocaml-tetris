type t =
  | Black
  | Red
  | Green
  | Blue
  | Yellow
  | Magenta
  | White
  | Cyan
  | Shade of float * t
  | Dark of t
  | Light of t
  | Mix of t * float * t

let scale x (r, g, b) = (x *. r, x *. g, x *. b)

let mix x (r1, g1, b1) (r2, g2, b2) =
  let y = 1.0 -. x in
  (x *. r1 +. y *. r2,
   x *. g1 +. y *. g2,
   x *. b1 +. y *. b2)

let rec rgb = function
  | Black -> (0.0, 0.0, 0.0)
  | Red -> (1.0, 0.0, 0.0)
  | Green -> (0.0, 1.0, 0.0)
  | Blue -> (0.0, 0.0, 1.0)
  | Yellow -> (1.0, 1.0, 0.0)
  | Magenta -> (1.0, 0.0, 1.0)
  | Cyan -> (0.0, 1.0, 1.0)
  | White -> (1.0, 1.0, 1.0)
  | Dark c -> scale 0.9 (rgb c)
  | Light c -> mix 0.9 (rgb c) (1.0, 1.0, 1.0)
  | Mix(c1, x, c2) -> mix x (rgb c1) (rgb c2)
  | Shade(x, c) ->
    if x < 0.0 then
      scale (max 0.0 (1.0 -. x)) (rgb c)
    else
      mix x (1.0, 1.0, 1.0) (rgb c)

let tweak c =
  let shade_variation = 0.100 in
  let n = (Random.float (2.0 *. shade_variation)) -. shade_variation in
  Shade(n, Dark(c))
