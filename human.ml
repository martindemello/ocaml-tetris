(* Human *)
(* $Id: human.ml,v 1.3 2003/11/18 22:51:09 berke Exp $ *)
(* vim:set ts=4: *)
(* By Oguz Berke DURAK *)

module Color =
  struct
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
    | Mix of t * float * t let scale x (r,g,b) = (x *. r, x *. g, x *. b)

	let mix x (r1,g1,b1) (r2,g2,b2) = let y = 1.0 -. x in (x *. r1 +. y *. r2,
														   x *. g1 +. y *. g2,
														   x *. b1 +. y *. b2)
	let rec rgb_of_color = function
	  | Black -> (0.0,0.0,0.0)
	  | Red -> (1.0,0.0,0.0)
	  | Green -> (0.0,1.0,0.0)
	  | Blue -> (0.0,0.0,1.0)
	  | Yellow -> (1.0,1.0,0.0)
	  | Magenta -> (1.0,0.0,1.0)
      | Cyan -> (0.0,1.0,1.0)
	  | White -> (1.0,1.0,1.0)
	  | Dark c -> scale 0.9 (rgb_of_color c)
	  | Light c -> mix 0.9 (rgb_of_color c) (1.0,1.0,1.0) 
	  | Mix(c1,x,c2) -> mix x (rgb_of_color c1) (rgb_of_color c2)
	  | Shade(x,c) ->
		  if x < 0.0 then
			scale (max 0.0 (1.0 -. x)) (rgb_of_color c)
		  else
			mix x (1.0,1.0,1.0) (rgb_of_color c)
  end

module Display =
  struct
	type command =
		Fill_rectangle of Color.t * int * int * int * int
	  | Draw_string of Color.t * int * int * string
  end

module Keypad =
  struct
	type t =
		Left | Right | Up | Down |
		A | B | Start | Select | L | R
	type event = Pressed of t | Released of t
  end

