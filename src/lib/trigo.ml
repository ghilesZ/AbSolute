(* This modules defines sound operators for trogonometrical functions *)

module B = Bound_float

(********************)
(* PI APPROXIMATION *)
(********************)

(* pi approximation with double precision *)

let pi_down = 3.14159265358979312 (* closest smaller float than pi *)
(* real pi  = 3.1415926535 89793238462.......... *)
let pi_up   = 3.14159265358979356 (* closest bigger float than pi *)

let pi_itv = pi_down,pi_up


(*********************)
(* SIN APPROXIMATION *)
(*********************)

(* over-approximation of sin x *)
let sin_up x = sin x

(* under-approximation of sin x *)
let sin_down x =
  let a = sin (x+.pi_up) and b =  sin (x+.pi_down) in
  -. (max a b)

(*********************)
(* COS APPROXIMATION *)
(*********************)

(* over-approximation of cos x *)
let cos_up x = cos x

(* under-approximation of cos x *)
let cos_down x =
  let a = cos (x+.pi_up) and b =  cos (x+.pi_down) in
  -. (max a b)

(************************)
(* ARCTAN APPROXIMATION *)
(************************)

let atan_down x = atan x

let atan_up x = -. (atan (-.x))

(**********************************)
(* ARCCOS IN [-1;1] APPROXIMATION *)
(**********************************)

let acos_up r =
  if -1. <= r && r <= 1. then Bot.Nb (acos r)
  else Bot.Bot

let acos_down r =
  if -1. <= r && r <= 1. then Bot.Nb (-. (acos (-.r)))
  else Bot.Bot

(****************************************************)
(*               INTERVAL COMPUTATION               *)
(****************************************************)

type itv = float * float

let print fmt (l,u) = Format.fprintf fmt
                                     "[%a; %a]"
                                     Format.pp_print_float l
                                     Format.pp_print_float u

(* constructors and conversions *)
let to_int (a,b) = (int_of_float a),(int_of_float b)

open B

let ((pi_down,pi_up) as pi_itv) = (of_float_down pi_down), (of_float_up pi_up)
let pihalf_down      = div_down pi_down two
let pihalf_up        = div_up pi_up two
let twopiup          = mul_up two pi_up
let twopidown        = mul_down two pi_down
let threehalfpi_down = mul_down pihalf_down (of_int_down 3)
let threehalfpi_up   = mul_up pihalf_up (of_int_up 3)

let pihalf_itv       = pihalf_down, pihalf_up
let twopi_itv        = twopidown, twopiup
let threehalfpi_itv  = threehalfpi_down, threehalfpi_up

(* trigonometry on intervals *)

(* the type of monotony of a function on a given interval *)

type monotony = Incr              (* strictly increasing*)
              | Decr              (* stricly decreasing*)
              | IncrDecr of float (* IncrDecr(a) means than the function f is
                                    increasing until x, and f(x) = a.
                                    then f decreases *)
              | DecrIncr of float (* DecrIncr(a) means than the function f is
                                    decreasing until x, and f(x) = a.
                                    then f increases *)

(* bring the interval to [0;4pi[
   - a < b
   - interval size should be smaller than 3pi/2, raise Exit if not *)
let normalize (a,b) =
  let rec aux a b =
    if threehalfpi_down <= (sub_up b a) then raise Exit
    else if twopiup <= a then aux (sub_down a twopiup) (sub_up b twopidown)
    else if zero <= a then a,b
    else aux (add_down a twopidown) (add_up b twopiup)
  in aux a b

let meet (a,b) (x,y) =
  a < y && x < b

(* return the monotony of the cosinus function on the given itv. Raise exit if
   the itv is bigger than 3pi/2 anyway *)
let cosmonotony itv =
  try let a,b = normalize itv in
      if meet (a,b) twopi_itv then
        IncrDecr(1.)
      else if meet (a,b) pi_itv then
        DecrIncr(-1.)
      else
        (* so it should be monoton *)
        if meet (pi_down,twopiup) (a,b) then Incr
        else Decr
  with Exit -> raise Exit

(* return the monotony of the sinus function on the given itv *)
let sinmonotony itv =
  try let a,b = normalize itv in
      if meet (a,b) pihalf_itv then
        IncrDecr(1.)
      else if meet (a,b) threehalfpi_itv then
        DecrIncr(-1.)
      else
        (* so it should be monoton *)
        if meet (pi_down,twopiup) (a,b) then Incr
        else Decr
  with Exit -> raise Exit

(* given an interval i, and a function f and its monotony,
   return the image of i by f *)
let itv ((a,b) as itv) monotony f_down f_up =
  match monotony itv with
  | Incr     -> (f_down a),(f_up b)
  | Decr     -> (f_down b),(f_up a)
  | IncrDecr max ->
     let a' = f_down a and b'= f_down b in
     if a' < b' then (a',max) else (b',max)
  | DecrIncr min ->
     let a' = f_up a and b'= f_up b in
     if a' < b' then (min,b') else (min,a')
  | exception Exit -> (-1.,1.)

(* cosinus of an interval *)
let cos_itv i = itv i cosmonotony cos_down cos_up

(* sinus of an interval *)
let sin_itv i = itv i sinmonotony sin_down sin_up

(* interval acos *)
let acos_itv (l,h) =
  if  h < -1. || 1. < l then Bot.Bot
  else
    let is_minus_one = l < -1.
    and is_plus_one =  1. < h in
    match (is_minus_one, is_plus_one) with
    | true, true -> Bot.Nb (0., pi_up)
    | true, false -> Bot.Nb ((acos_down h), pi_up)
    | false, true -> Bot.Nb (0., (acos_up l))
    | false, false -> Bot.Nb ((acos_down h), (acos_up l))
