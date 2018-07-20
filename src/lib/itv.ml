(* Generic intervals.
   Working with bounds that may induce rounding errors.
   Can be instantiated with any bound type. *)

open Bot
open Bound_sig

module Itv(B:BOUND) = struct

  (************************************************************************)
  (*                                TYPES                                 *)
  (************************************************************************)


  (* interval bound (possibly -oo or +oo *)
  type bound = B.t

  (* an interval is a pair of bounds (lower,upper);
     intervals are always non-empty: lower <= upper;
     functions that can return an empty interval return it as Bot *)
  type t = bound * bound

  (* not all pairs of rationals are valid intervals *)
  let validate ((l,h):t) : t =
    if B.gt l h then
      invalid_arg
        (Format.asprintf "itv.validate: %f %f"
           (B.to_float_down l)
           (B.to_float_up h))
    else l,h

  (* maps empty intervals to explicit bottom *)
  let check_bot ((l,h):t) : t bot =
    if B.leq l h then Nb (l,h) else Bot

  (************************************************************************)
  (*                     CONSTRUCTORS AND CONSTANTS                       *)
  (************************************************************************)

  let of_bound (x:B.t) : t = validate (x,x)

  let zero : t = of_bound B.zero

  let one : t = of_bound B.one

  let minus_one : t = of_bound B.minus_one

  let positive : t = B.zero, B.inf

  let negative : t = B.minus_inf, B.zero

  let of_bounds (l:bound) (h:bound) = validate (l,h)

  let of_ints (l:int) (h:int) : t = of_bounds (B.of_int_down l) (B.of_int_up h)

  let of_int (x:int) = of_ints x x

  let of_floats (l:float) (h:float) : t = of_bounds (B.of_float_down l) (B.of_float_up h)

  let of_float (x:float) = of_floats x x

  let float_hull (x:float) (y:float) = min x y, max x y

  (************************************************************************)
  (*                            PRINTING                                  *)
  (************************************************************************)

  (* printing *)
  let print fmt ((l,h):t) : unit =
    Format.fprintf fmt "[%a;%a]" B.print l B.print h

  (************************************************************************)
  (*                          SET-THEORETIC                               *)
  (************************************************************************)

  (* operations *)
  (* ---------- *)

  let join ((l1,h1):t) ((l2,h2):t) : t =
    B.min l1 l2, B.max h1 h2

  let meet ((l1,h1):t) ((l2,h2):t) : t bot =
    check_bot (B.max l1 l2, B.min h1 h2)


  (* predicates *)
  (* ---------- *)

  let equal ((l1,h1):t) ((l2,h2):t) : bool =
    B.equal l1 l2 && B.equal h1 h2

  let subseteq ((l1,h1):t) ((l2,h2):t) : bool =
    B.geq l1 l2 && B.leq h1 h2

  let contains_float ((l,h):t) (x:float) : bool =
    B.leq l (B.of_float_down x) && B.leq (B.of_float_up x) h

  let intersect ((l1,h1):t) ((l2,h2):t) : bool =
    B.leq l1 h2 && B.leq l2 h1

  let is_singleton ((l,h):t) : bool =  B.equal l h

  (* mesure *)
  (* ------ *)

  (* length of the intersection (>= 0) *)
  let overlap ((l1,h1):t) ((l2,h2):t) : B.t  =
    B.max B.zero (B.sub_up (B.min h1 h2) (B.max l1 l2))

  let range ((l,h):t) : B.t =
    B.sub_up h l

  let magnitude ((l,h):t) : B.t =
    B.max (B.abs l) (B.abs h)

  (* split *)
  (* ----- *)

  (* split priority *)
  let score itv = range itv |> B.to_float_up

  (* finds the mean of the interval *)
  let mean ((l,h):t) : B.t list = [B.div_up (B.add_up l h) B.two]

  (* splits in two, around the middle *)
  let split ((l,h) as i :t) : t list =
    let rec aux acc cur bounds =
      match bounds with
      |  hd::tl ->
	       let itv = validate (cur,hd) in
	       aux (itv::acc) hd tl
      | [] ->
	       let itv = validate (cur,h) in
	       itv::acc
    in aux [] l (mean i)

  let prune (((l,h) as a):t) (((l',h') as b):t) : t list * t  =
    if subseteq a b then [],a
    else
      let epsilon = B.of_float_up 0.00001 in
      let h'_eps = B.add_up h' epsilon and l'_eps = B.add_down l' (B.neg epsilon) in
      (* It may not be worth to use the pruning to win a very small step *)
      let step = B.of_float_up 0.1 in
      let h_step = B.add_up h'_eps step and l_step = B.sub_down l'_eps step in
      match (B.gt l_step l),(B.lt h_step h) with
      | true , true  -> [(l,l'_eps);(h'_eps,h)],(l'_eps,h'_eps)
      | true , false -> [(l,l'_eps)],(l'_eps,h)
      | false, true  -> [(h'_eps,h)],(l,h'_eps)
      | false, false -> [],(l,h)

  (************************************************************************)
  (* INTERVAL ARITHMETICS (FORWARD EVALUATION) *)
  (************************************************************************)

  let neg ((l,h):t) : t =
    B.neg h, B.neg l

  let abs ((l,h):t) : t =
    if contains_float (l,h) 0. then B.zero, B.max (B.abs l) (B.abs h)
    else if B.gt (B.abs l) (B.abs h) then of_bounds (B.abs h) (B.abs l)
    else of_bounds (B.abs l) (B.abs h)

  let add ((l1,h1):t) ((l2,h2):t) : t =
    B.add_down l1 l2, B.add_up h1 h2

  let sub ((l1,h1):t) ((l2,h2):t) : t =
    B.sub_down l1 h2, B.sub_up h1 l2

  let bound_mul_up   = B.bound_mul B.mul_up
  let bound_mul_down = B.bound_mul B.mul_down

  let mix4 up down ((l1,h1):t) ((l2,h2):t) =
    B.min (B.min (down l1 l2) (down l1 h2)) (B.min (down h1 l2) (down h1 h2)),
    B.max (B.max (up   l1 l2) (up   l1 h2)) (B.max (up   h1 l2) (up   h1 h2))

  let mul =
    mix4 bound_mul_up bound_mul_down

  let bound_div_up   = B.bound_div B.div_up
  let bound_div_down = B.bound_div B.div_down

  (* helper: assumes i2 has constant sign *)
  let div_sign =
    mix4 bound_div_up bound_div_down

  (* return valid values (possibly Bot) + possible division by zero *)
  let div (i1:t) (i2:t) : t bot =
    (* split into positive and negative dividends *)
    let pos = (lift_bot (div_sign i1)) (meet i2 positive)
    and neg = (lift_bot (div_sign i1)) (meet i2 negative) in
    (* joins the result *)
    join_bot2 join pos neg

  (* interval square root *)
  let sqrt ((l,h):t) : t bot =
    if B.sign h < 0 then Bot else
    let l = B.max l B.zero in
    Nb (B.sqrt_down l, B.sqrt_up h)

  (* deprecated : use the interval version instead*)
  let ln10 = B.of_float_up 2.3025850

  (* interval exp *)
  let exp (l,h) = (B.exp_down l,B.exp_up h)

  (* interval ln *)
  let ln (l,h) =
    if B.leq h B.zero then Bot
    else if B.leq l B.zero then Nb (B.minus_inf,B.ln_up h)
    else Nb (B.ln_down l,B.ln_up h)

  (* interval log *)
  let log itv =
    let itv' = ln itv in
    match itv' with
    | Bot -> Bot
    | Nb (l', h') -> (div (l',h') (of_bound ln10))

  (* powers *)
  let pow ((il,ih):t) ((l,h):t) =
    if l=h && B.floor l = l then
      let p = B.to_float_down l |> int_of_float in
      match p with
      | 0 -> one
      | 1 -> (il, ih)
      | x when x > 1 && p mod 2 = 1 -> (B.pow_down il p, B.pow_up ih p)
      | x when x > 1 && B.even l ->
         if B.leq il B.zero && B.geq ih B.zero then
	         (B.zero, B.max (B.pow_up il p) (B.pow_up ih p))
         else if B.geq il B.zero then
	         (B.pow_down il p, B.pow_up ih p)
         else (B.pow_down ih p, B.pow_up il p)
      | x -> Tools.fail_fmt "cant handle negatives powers : %i" x
    else Tools.fail_fmt "cant handle non_singleton powers : %a" print (l,h)

  (* nth-root *)
  let n_root ((il,ih):t) ((l,h):t) =
    if B.equal l h && B.floor l = l then
      let p = B.to_float_down l |> int_of_float in
      match p with
      | 1 -> Nb (il, ih)
      | 2 -> Nb (B.sqrt_down il, B.sqrt_up ih)
      | x when x > 1 && B.odd l ->
	       Nb (B.root_down il p, B.root_up ih p)
      | x when x > 1 && B.even l ->
         if B.lt ih B.zero then Bot
         else if B.leq il B.zero then Nb (B.neg (B.root_up ih p), B.root_up ih p)
         else
           Nb (B.min (B.neg (B.root_down il p)) (B.neg (B.root_down ih p)),
               B.max (B.root_up il p) (B.root_up ih p))
      | x -> Tools.fail_fmt "can only handle stricly positive roots : %i" x
    else Tools.fail_fmt "cant handle non_singleton roots: %a" print (l,h)

  (* min *)
  let min ((l1, u1):t) ((l2, u2):t) =
    validate (B.min l1 l2, B.min u1 u2)

  (* max *)
  let max ((l1, u1):t) ((l2, u2):t) =
    validate (B.max l1 l2, B.max u1 u2)

  (***********************)
  (* FUNCTION EVALUATION *)
  (***********************)

  (** runtime functions **)
  let eval_fun name args : t bot =
    let arity_1 (f: t -> t) : t bot =
       match args with
       | [i] -> Nb (f i)
      | _ -> Tools.fail_fmt "%s expect one argument" name
    in
    let arity_1_bot (f: t -> t bot) : t bot =
      match args with
      | [i] -> f i
      | _ -> Tools.fail_fmt "%s expect one argument" name
    in
    let arity_2 (f: t -> t -> t) : t bot  =
      match args with
      | [i1;i2] -> Nb (f i1 i2)
      | _ -> Tools.fail_fmt "%s expect two arguments" name
    in
    let arity_2_bot (f: t -> t -> t bot) : t bot  =
      match args with
      | [i1;i2] ->
         (match f i1 i2 with
          | Bot -> Bot
          | Nb(i) -> Nb i)
      | _ -> Tools.fail_fmt "%s expect two arguments" name
    in
    match name with
    | "pow"   -> arity_2 pow
    | "nroot" -> arity_2_bot n_root
    | "sqrt"  -> arity_1_bot sqrt
    | "exp"   -> arity_1 exp
    | "ln"    -> arity_1_bot ln
    | "log"   -> arity_1_bot log
    | "max"   -> arity_2 max
    | "min"   -> arity_2 min
    | s -> Tools.fail_fmt "unknown eval function : %s" s

  (************************************************************************)
  (* FILTERING (TEST TRANSFER FUNCTIONS) *)
  (************************************************************************)

  (* tests *)
  (* ----- *)

  let filter_leq ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
    merge_bot2 (check_bot (l1, B.min h1 h2)) (check_bot (B.max l1 l2, h2))

  let filter_lt ((l1,_) as i1:t) ((l2,h2) as i2:t) : (t*t) bot =
    if is_singleton i1 && is_singleton i2 && B.equal l1 l2 then Bot
    else if B.leq h2 l1 then Bot
    else filter_leq i1 i2

  let filter_eq (i1:t) (i2:t) : (t*t) bot =
    lift_bot (fun x -> x,x) (meet i1 i2)

  let filter_neq ((l1,_) as i1:t) ((l2,_) as i2:t) : (t*t) bot =
    if is_singleton i1 && is_singleton i2 && B.equal l1 l2 then Bot
    else Nb (i1,i2)

  (* arithmetic *)
  (* --------- *)

  (* r = -i => i = -r *)
  let filter_neg (i:t) (r:t) : t bot =
    meet i (neg r)

  let filter_abs ((il,ih) as i:t) ((rl,rh) as r:t) : t bot =
    if B.sign il >= 0 then meet i r
    else if B.sign ih <= 0 then meet i (neg r)
    else meet i (B.neg rh, rh)

  (* r = i1+i2 => i1 = r-i2 /\ i2 = r-i1 *)
  let filter_add (i1:t) (i2:t) (r:t) : (t*t) bot =
    merge_bot2 (meet i1 (sub r i2)) (meet i2 (sub r i1))

  (* r = i1-i2 => i1 = i2+r /\ i2 = i1-r *)
  let filter_sub (i1:t) (i2:t) (r:t) : (t*t) bot =
    merge_bot2 (meet i1 (add i2 r)) (meet i2 (sub i1 r))

  (* r = i1*i2 => (i1 = r/i2 \/ i2=r=0) /\ (i2 = r/i1 \/ i1=r=0) *)
  let filter_mul (i1:t) (i2:t) (r:t) : (t*t) bot =
    merge_bot2
      (if contains_float r 0. && contains_float i2 0. then Nb i1
      else match (div r i2) with Bot -> Bot | Nb x -> meet i1 x)
      (if contains_float r 0. && contains_float i1 0. then Nb i2
      else match (div r i1) with Bot -> Bot | Nb x -> meet i2 x)

  (* r = i1/i2 => i1 = i2*r /\ (i2 = i1/r \/ i1=r=0) *)
  let filter_div (i1:t) (i2:t) (r:t) : (t*t) bot =
    merge_bot2
      (meet i1 (mul i2 r))
      (if contains_float r 0. && contains_float i1 0. then Nb i2
      else match (div i1 r) with Bot -> Bot | Nb x -> meet i2 x)

  (* r = sqrt i => i = r*r or i < 0 *)
  let filter_sqrt ((il,ih) as i:t) ((rl,rh):t) : t bot =
    let rr = B.mul_down rl rl, B.mul_up rh rh in
    if B.sign il >= 0 then meet i rr
    else meet i (B.minus_inf, snd rr)

  (* r = exp i => i = ln r *)
  let filter_exp i r =
    meet_bot meet i (ln r)

  (* r = ln i => i = exp r *)
  let filter_ln i r =
    meet i (exp r)

  (* r = log i => i = *)
  let filter_log i r = Tools.fail_fmt "todo filter_log"

  (* r = i ** n => i = nroot r *)
  let filter_pow (i:t) n (r:t) =
    merge_bot2 (meet_bot meet i (n_root r n)) (Nb n)

  (* r = nroot i => i = r ** n *)
  let filter_root i r n =
    merge_bot2 (meet i (pow r n)) (Nb n)

  (* r = min (i1, i2) *)
  let filter_min (l1, u1) (l2, u2) (lr, ur) =
    merge_bot2
      (check_bot ((B.max l1 lr), (B.max u1 ur)))
      (check_bot ((B.max l2 lr), (B.max u2 ur)))

  (* r = max (i1, i2) *)
  let filter_max (l1, u1) (l2, u2) (lr, ur) =
    merge_bot2
      (check_bot ((B.min l1 lr), (B.min u1 ur)))
      (check_bot ((B.min l2 lr), (B.min u2 ur)))

  (* r = f(x0,x1,...,xn) *)
  let filter_fun name args r : (t list) bot =
    let arity_1 (f: t -> t -> t bot) : (t list) bot =
      match args with
      | [i] ->
         (match f i r with
         | Bot -> Bot
         | Nb i -> Nb [i])
      | _ -> Tools.fail_fmt "%s expect one argument" name
    in
    let arity_2 (f: t -> t -> t -> (t*t) bot) : (t list) bot  =
      match args with
      | [i1;i2] ->
         (match f i1 i2 r with
          | Bot -> Bot
          | Nb(i1,i2) -> Nb[i1;i2])
      | _ -> Tools.fail_fmt "%s expect two arguments" name
    in
    match name with
    | "sqrt" -> arity_1 filter_sqrt
    | "exp"  -> arity_1 filter_exp
    | "ln"   -> arity_1 filter_ln
    | "max"  -> arity_2 filter_max
    | "min"  -> arity_2 filter_min
    | s -> Tools.fail_fmt "unknown filter function : %s" s

  let to_float_range (l,h) =
    (B.to_float_down l),(B.to_float_up h)

  (* generate a random float between l and h *)
  let spawn (l,h) =
    let r = Random.float 1. in
    B.add_up l (B.mul_up (B.sub_up h l) (B.of_float_up r))

end

module ItvF = Itv(Bound_float)
