(*
  This modules defines sound operators for trogonometrical functions
  It uses floating point precision
  It plugs itself over a interval arithmetic module
  The interface is functional.
*)

open Bot

module F = Bound_float

module Make (I:Itv_sig.ITV) = struct

  (* All the classical interval computations are keeped *)
  include I


  (********************)
  (* PI APPROXIMATION *)
  (********************)

  (* pi approximation (double precision) *)

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

  (*********************)
  (* TAN APPROXIMATION *)
  (*********************)

  (* TODO: check division by zero *)

  (* over-approximation of tan x *)
  let tan_up x = tan x

  (* under-approximation of tan x *)
  let tan_down x =  -. (tan (-. x))

  (************************)
  (* ARCTAN APPROXIMATION *)
  (************************)

  let atan_down x = atan x

  let atan_up x = -. (atan (-.x))

  (************************)
  (* ARCCOS APPROXIMATION *)
  (************************)

  let acos_up r =
    if -1. <= r && r <= 1. then Nb (acos r)
    else Bot

  let acos_down r =
    if -1. <= r && r <= 1. then Nb (-. (acos (-.r)))
    else Bot

  (************************)
  (* ARCSIN APPROXIMATION *)
  (************************)

  let asin_up r =
    if -1. <= r && r <= 1. then Nb (asin r)
    else Bot

  let asin_down r =
    if -1. <= r && r <= 1. then Nb (-. (asin (-.r)))
    else Bot


  (****************************************************)
  (*               INTERVAL COMPUTATION               *)
  (****************************************************)

  type itv = float * float

  let print fmt (l,u) = Format.fprintf fmt
                                       "[%a; %a]"
                                       Format.pp_print_float l
                                       Format.pp_print_float u

  open F

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

  (* the type of monotony of a function on a given interval: *)
  (* - Incr means strictly increasing*)
  (* - Decr means stricly decreasing*)
  (* - Change means than the function f is not monotonic on the given interval.
       the boolean indicates if the function is firstly increasing (true)
       or decreasing(false). The float list are the images of where the
       monotony changes.
       ex : Change(true,[f1,f2,f3 ...]) means that f is first increasing
       than it changes its monotony at some point x1 and f(x1) = f1. then
       it descreases until a point x2 such that f(x2) = f2 etc. *)
  type monotony = Incr
                | Decr
                | Change of bool * float list

  (* bring the interval to [0;4pi[ (the lower bound in [0;2pi[ )
   - a < b
   - interval size should be smaller than 3pi/2, raise Exit if not *)
  let normalize ((a:float),(b:float)) =
    let rec aux a b =
      if threehalfpi_down <= (sub_up b a) then raise Exit
      else if twopiup <= a then aux (sub_down a twopiup) (sub_up b twopidown)
      else if zero <= a then a,b
      else aux (add_down a twopidown) (add_up b twopiup)
    in aux a b

   (* return the monotony of the sinus function on the given itv *)
  (* monotony is computed according to the value of the derivative *)
  let cosmonotony (a,b) =
    if threehalfpi_down <= (sub_up b a) then raise Exit
    else
      let m_sin_a = -. (sin a) and m_sin_b = -. (sin b) in
      if m_sin_a < 0. then
        if m_sin_b < 0. then Decr
        else Change(false,[-1.])
      else
        if m_sin_b > 0. then Incr
        else Change(true,[1.])

  (* return the monotony of the sinus function on the given itv *)
  let sinmonotony (a,b) = cosmonotony ((a-.pihalf_down),(b+.pihalf_down))

  (* given an interval i, and a function f and its monotony,
   return the image of i by f *)
  let itv monotony f_down f_up ((a,b) as itv) =
    match monotony itv with
    | Incr     -> (f_down a),(f_up b)
    | Decr     -> (f_down b),(f_up a)
    | Change (true, [max]) ->
       let a' = f_down a and b'= f_down b in
       if a' < b' then (a',max) else (b',max)
    | Change(false, [min]) ->
       let a' = f_up a and b'= f_up b in
       if a' < b' then (min,b') else (min,a')
    | _ (* change monotony more than once *)->  (-1.,1.)
    | exception Exit -> (-1.,1.)

  (* Use float to do a computation then go back to B.t *)
  let bound_float_convert i f =
    let i = I.to_float_range i in
    let (l,u) = f i
    in I.of_floats l u

  (****************************************************)
  (* INTERVAL EVALUTATION OF TTRIGONOMETRIC FUNCTIONS *)
  (****************************************************)

  (* cosinus of an interval *)
  let cos_itv i =
    bound_float_convert i (itv cosmonotony cos_down cos_up)

   (* interval acos *)
  let acos_itv i =
    let (l,h) = I.to_float_range i in
    if 1. < l ||  h < -1. then Bot
    else
      let l = if 1. < h then 0. else debot (acos_down h)
      and u =  if l < -1. then pi_up else debot (acos_up l)
      in Nb(I.of_floats l u)

  (* sinus of an interval *)
  let sin_itv i =
    bound_float_convert i (itv sinmonotony sin_down sin_up)

  (* interval asin *)
  let asin_itv i =
    let (l,h) = I.to_float_range i in
    if 1. < l ||  h < -1. then Bot
    else
      let l = if 1. < h then -.pihalf_down else debot (asin_down h)
      and u = if l < -1. then pihalf_up else debot (asin_up l)
      in Nb(I.of_floats l u)

  (* tangent of an interval *)
  let tan_itv i = I.div  (sin_itv i) (cos_itv i)

  (* we augment the function evaluator to add to it the trigonometrical stuff *)
  let eval_fun name args =
    let arity_1 (f: I.t -> I.t) : I.t bot =
      match args with
      | [i] -> Nb (f i)
      | _ -> failwith (Format.sprintf "%s expect one argument" name)
    in
    let arity_1_bot (f: I.t -> I.t bot) : I.t bot =
      match args with
      | [i] -> f i
      | _ -> failwith (Format.sprintf "%s expect one argument" name)
    in
    match name with
    | "cos" -> arity_1 cos_itv
    | "sin"   -> arity_1 sin_itv
    | "acos"  -> arity_1_bot acos_itv
    | "asin"  -> arity_1_bot asin_itv
    | _ -> I.eval_fun name args

  (***************************************)
  (* FILTERING (TEST TRANSFER FUNCTIONS) *)
  (***************************************)

   (* r = cos i => i = arccos r *)
  let filter_cos (i:I.t) (r:I.t) : I.t bot =
    (* TODO:improve precision *)
    (* let acos_r = acos_itv r in *)
    Nb i

  (* r = sin i => i = arcsin r *)
  let filter_sin (i:I.t) (r:I.t) : I.t bot =
    (* TODO:improve precision *)
    (* let asin_r = asin_itv r in *)
    Nb i

  (* r = asin i => i = sin r *)
  let filter_asin i r =
    I.meet i (sin_itv r)

  (* r = acos i => i = cos r *)
  let filter_acos i r =
    I.meet i (cos_itv r)

  (* we augment the function filterer to add to it the trigonometrical stuff *)
  let filter_fun name args r : (I.t list) bot =
   let arity_1 (f: I.t -> I.t -> I.t bot) : (I.t list) bot =
      match args with
      | [i] ->
         (match f i r with
         | Bot -> Bot
         | Nb i -> Nb [i])
      | _ -> failwith (Format.sprintf "%s expect one argument" name)
    in
    match name with
    | "cos"  -> arity_1 filter_cos
    | "sin"  -> arity_1 filter_sin
    | "acos" -> arity_1 filter_acos
    | "asin" -> arity_1 filter_asin
    | _ -> I.filter_fun name args r
end

module ItvF = Make(Itv.ItvF)
