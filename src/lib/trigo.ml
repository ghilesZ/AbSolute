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
  (* real pi  = 3.141592653589793238462.......... *)
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

  type float_itv = float * float

  let print_fitv fmt (l,u) = Format.fprintf fmt
                                       "[%a; %a]"
                                       Format.pp_print_float l
                                       Format.pp_print_float u

  let pihalf_down   = F.div_down pi_down F.two
  let pihalf_up     = F.div_up pi_up F.two
  let twopiup       = F.mul_up F.two pi_up
  let twopidown     = F.mul_down F.two pi_down

  let pihalf_fitv    = (pihalf_down, pihalf_up)
  let pi_fitv        = (F.of_float_down pi_down), (F.of_float_up pi_up)
  let twopi_fitv     = (twopidown, twopiup)

  let fitv_to_i (f1,f2) = I.of_floats f1 f2

  let pihalf_itv = fitv_to_i pihalf_fitv
  let pi_itv     = fitv_to_i pi_fitv
  let twopi_itv  = fitv_to_i twopi_fitv

  (* the type of monotony of a function on a given interval: *)
  (* - Incr means strictly increasing*)
  (* - Decr means stricly decreasing*)
  (* - Change means than the function f is not monotonic on the given interval.
       the boolean indicates if the function is firstly increasing (true)
       or decreasing(false) *)
  type monotony = Incr
                | Decr
                | Change of bool

  let print_monotony fmt mon =
    let s =
      match mon with
      | Incr -> "increasing"
      | Decr -> "decreasing"
      | Change true -> "increasing then changes"
      | Change false -> "decreasing then changes"
    in
    Format.fprintf fmt "%s" s

  (* given an interval i, and a function f and its monotony,
   return the image of i by f *)
  let itv monotony f_down f_up (a,b) =
    match monotony with
    | Incr     -> (f_down a),(f_up b)
    | Decr     -> (f_down b),(f_up a)
    | Change i ->
       if i then
         let a' = f_down a and b'= f_down b in
         (min a' b',1.)
       else
         let a' = f_up a and b'= f_up b in
         (-1.),(max a' b')

  (***************************************************)
  (* INTERVAL EVALUTATION OF TRIGONOMETRIC FUNCTIONS *)
  (***************************************************)

  let cosmonotony (a,b) =
    (* returns the monotony, supposes that the monotony changes at most once *)
    let mono_once (a,b) =
      let m_sin_a = -. (sin a) and m_sin_b = -. (sin b) in
      if m_sin_a = 0. then
        if m_sin_b < 0. then Decr
        else Incr
      else
      if m_sin_a < 0. then
        if m_sin_b <= 0. then Decr
        else Change(false)
      else
        if m_sin_b >= 0. then Incr
        else Change(true)
    in
    let rec mono (a,b) =
      if a > b then assert false;
      let range = (F.sub_up b a) in
      if twopidown <= range then raise Exit
      else
        if pi_down <= range then
          (* monotony changes at least once, at most twice *)
          let mid = a +. (b-.a)/.2. in
          match (mono (a,mid)),(mono (mid,b)) with
          | Incr,Incr -> Incr
          | Decr,Decr -> Decr
          | Incr,Decr -> Change (true)
          | Decr,Incr -> Change (false)
          | Change (_), Change (_) -> raise Exit
          | (Change (_) as x), _ | _,(Change (_) as x) -> x
        else
          (* monotony changes at most once *)
          mono_once (a,b)
    in mono (a,b)

  (* cosinus of an interval *)
  let cos_itv i =
    let (a,b) = I.to_float_range i in
    match cosmonotony (a,b) with
    | mon ->
       (* Format.printf "monotony is %a on %a\n%!" print_monotony mon print_fitv (a,b); *)
       fitv_to_i (itv mon cos_down cos_up (a,b))
    | exception Exit -> I.of_floats (-1.) 1.

  (* interval acos *)
  let acos_itv i =
    let (l,u) = I.to_float_range i in
    if 1. < l ||  u < -1. then Bot
    else
      let l' = if 1. < u then 0. else debot (acos_down u)
      and u' =  if l < -1. then pi_up else debot (acos_up l)
      in Nb(I.of_floats l' u')

  (* sinus of an interval *)
  let sin_itv i =
    cos_itv (I.sub i pihalf_itv)

  (* interval asin (arcos + arcsin = pi/2) *)
  let asin_itv i =
    match acos_itv i with
    | Bot -> Bot
    | Nb i -> Nb(I.sub pihalf_itv i)

  (* tangent of an interval *)
  let tan_itv i = fst (I.div (sin_itv i) (cos_itv i))

  (* atan of an interval *)
  let atan_itv i =
    let (l,u) = I.to_float_range i in
    fitv_to_i ((atan_down l),(atan_up u))

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
    | "cos"  -> arity_1 cos_itv
    | "sin"  -> arity_1 sin_itv
    | "acos" -> arity_1_bot acos_itv
    | "asin" -> arity_1_bot asin_itv
    | "tan"  -> arity_1_bot tan_itv
    | "atan" -> arity_1 atan_itv
    | _ -> I.eval_fun name args

  (***************************************)
  (* FILTERING (TEST TRANSFER FUNCTIONS) *)
  (***************************************)

  (* bring the interval to [0;4pi[ (the lower bound in [0;2pi[ )
     interval size should be smaller than pi, it raises Exit if not *)
  let normalize i =
    if twopidown <= I.range i then raise Exit
    else
      let (a,b) = I.to_float_range i in
      let nb_2pi = floor (F.div_down a twopidown) in
      let dist = I.mul (I.of_float nb_2pi) twopi_itv in
      let i' = I.sub i dist in
      if twopidown <= I.range i' then raise Exit
      else i',dist

  (* 0 < x < 2pi && cos(x) = r <=> x = arcos r || x = arcos(-r)+pi *)
  let arcos_0_2pi =
    let zero_pi   = I.of_floats 0. pi_up in
    let pi_2pi    = I.of_floats pi_down twopiup in
    let open Bot in
    fun itv result ->
    match (I.intersect itv zero_pi),(I.intersect itv pi_2pi) with
    | false,true  -> Bot.lift_bot (I.add pi_itv) (acos_itv (I.neg result))
    | true,false  -> acos_itv result
    | true,true   ->
       Bot.lift_bot (I.add pi_itv) (acos_itv (I.neg result)) |>
         Bot.join_bot2 I.join (acos_itv result)
    | false,false -> Bot

  (* r = cos i => i mod 2pi = arccos r *)
  let filter_cos =
    let zero_2pi   = I.of_floats 0. twopiup in
    let twopi_4pi  = I.add twopi_itv zero_2pi in
    fun (i:I.t) (r:I.t) : I.t bot ->
    try
      let i',delta  = normalize i in
      let first_part = Bot.lift_bot (I.add delta) (arcos_0_2pi i' r) in
      let second_part =
      match (I.meet i' twopi_4pi) with
      | Nb x ->
         let x' = I.sub x twopi_itv in
         Bot.lift_bot (I.add (I.add delta twopi_itv)) (arcos_0_2pi x' r)
      | Bot -> Bot
      in Bot.join_bot2 I.join first_part second_part
    with
    | Exit -> Nb i

  (* r = sin i => i mod 2pi = arcsin r *)
  let filter_sin (i:I.t) (r:I.t) : I.t bot =
    (* try *)
    (*   let i',delta =  normalize i in *)
    (*   let ri' = asin_itv i' in *)
    (*   meet i (I.add (debot ri') delta) *)
    (* with *)
    (* | Exit | Bot_found -> *) Nb i

  (* r = tan i => i mod 2pi = arctan r *)
  let filter_tan (i:I.t) (r:I.t) : I.t bot =
    (* try *)
    (*   let i',delta = normalize i in *)
    (*   let ri' = atan_itv i' in *)
    (*   meet i (I.add ri' delta) *)
    (* with *)
    (* | Exit | Bot_found -> *) Nb i

  (* r = asin i => i = sin r *)
  let filter_asin i r =
    I.meet i (sin_itv r)

  (* r = acos i => i = cos r *)
  let filter_acos i r =
    I.meet i (cos_itv r)

  (* r = atan i => i = tan r *)
  let filter_atan i r =
    I.meet i (atan_itv r)

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
    | "tan"  -> arity_1 filter_tan
    | "atan" -> arity_1 filter_atan
    | _ -> I.filter_fun name args r
end

module ItvF = Make(Itv.ItvF)
