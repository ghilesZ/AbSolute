(*
   Generic intervals.

   Can be instantiated with any bound type.
*)


open Bot
open Bound_sig

module Itv(B:BOUND) = struct

  (************************************************************************)
  (* TYPES *)
  (************************************************************************)


  (* interval bound (possibly -oo or +oo *)
  module B = B
  type bound = B.t

  (* an interval is a pair of bounds (lower,upper);
     intervals are always non-empty: lower <= upper;
     functions that can return an empty interval return it as Bot
   *)
  type t = bound * bound

  (* not all pairs of rationals are valid intervals *)
  let validate ((l,h):t) : t =
    match B.classify l, B.classify h with
    | B.INVALID,_ | _,B.INVALID  | B.MINF,_ | _,B.INF
    | _ when  B.gt l h -> invalid_arg "int.validate"
    | _ -> l,h


  (* maps empty intervals to explicit bottom *)
  let check_bot ((l,h):t) : t bot =
    if B.leq l h then Nb (l,h) else Bot


  (************************************************************************)
  (* CONSTRUCTORS AND CONSTANTS *)
  (************************************************************************)


  let of_bound (x:B.t) : t = validate (x,x)

  let zero : t = of_bound B.zero

  let one : t = of_bound B.one

  let minus_one : t = of_bound B.minus_one

  let top : t = B.minus_inf, B.inf

  let zero_one : t = B.zero, B.one

  let positive : t = B.zero, B.inf

  let negative : t = B.minus_inf, B.zero

  let of_bounds (l:bound) (h:bound) = validate (l,h)

  let of_ints (l:int) (h:int) : t = of_bounds (B.of_int_down l) (B.of_int_up h)

  let of_int (x:int) = of_ints x x

  (* let of_rats (l:Q.t) (h:Q.t) : t = of_bounds (B.of_rat_down l) (B.of_rat_up h) *)

  (* let of_rat (x:Q.t) = of_rats x x *)

  let of_floats (l:float) (h:float) : t = of_bounds (B.of_float_down l) (B.of_float_up h)

  let of_float (x:float) = of_floats x x

  let float_hull (x:float) (y:float) = min x y, max x y



  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)

    (*
  let to_string ((l,h):t) : string =
    Printf.sprintf "[%s;%s]" (B.to_string l) (B.to_string h)
     *)

  let to_string ((l,h):t) : string =
    Printf.sprintf "[%f;%f]" (B.to_float_down l) (B.to_float_up h)


  (* printing *)
  let print fmt ((l,h):t) =
    match (B.ceil l = l),(B.ceil h = h) with
    | true,true -> Format.fprintf fmt "[%0F;%0F]" (B.to_float_down l) (B.to_float_up h)
    | false,true -> Format.fprintf fmt "[%f;%0F]" (B.to_float_down l) (B.to_float_up h)
    | true,false -> Format.fprintf fmt "[%0F;%f]" (B.to_float_down l) (B.to_float_up h)
    | _ -> Format.fprintf fmt "[%f;%f]" (B.to_float_down l) (B.to_float_up h)

  let to_expr ((l, h):t) =
    ((Csp.GEQ, Csp.Cst(B.to_float_down l)),
     (Csp.LEQ, Csp.Cst(B.to_float_up h)))

  (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)


  (* operations *)
  (* ---------- *)

  let join ((l1,h1):t) ((l2,h2):t) : t =
    B.min l1 l2, B.max h1 h2

  (* returns None if the set-union cannot be exactly represented *)
  let union ((l1,h1):t) ((l2,h2):t) : t option =
    if B.leq l1 h2 && B.leq l2 h1 then Some (B.min l1 l2, B.max h1 h2)
    else None

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

  let is_finite x =
    B.classify x = B.FINITE

  let is_bounded ((l,h):t) =
    is_finite l && is_finite h

  let is_singleton ((l,h):t) : bool =
    is_finite l && B.equal l h

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

  (* find the mean of the interval;
     when a bound is infinite, then "mean" is some value strictly inside the
     interval
   *)
  let mean ((l,h):t) : B.t list =
    let res =
    match is_finite l, is_finite h with
    | true,true ->
      (* finite bounds: returns the actual mean *)
      B.div_up (B.add_up l h) B.two
    | true,false ->
      (* [l,+oo] *)
      if B.sign l < 0 then B.zero      (* cut at 0 if [l,+oo] contains 0 *)
      else if B.sign l = 0 then B.one  (* cut at 1 if [l,+oo] touches 0 *)
      else B.mul_up l B.two            (* cut at 2l if [l,+oo] is positive *)
    | false,true ->
        (* [-oo,h]: similar to [l,+oo] *)
        if B.sign h > 0 then B.zero
        else if B.sign h = 0 then B.minus_one
        else B.mul_down h B.two
    | false,false ->
        (* the mean of [-oo,+oo] is 0 *)
      B.zero
    in [res]

  (* splits in two, around m *)
  let split ((l,h):t) (m:bound list) : (t bot) list =
    let rec aux acc cur bounds =
      match bounds with
      |  hd::tl ->
	       let itv = check_bot (cur,hd) in
	       aux (itv::acc) hd tl
      | [] ->
	       let itv = check_bot (cur,h) in
	       itv::acc
    in aux [] l m

  let split_integer ((l,h):t) (m:bound list) : (t bot) list =
    let to_pair = ref l in
    let list =
      List.rev_map (fun e ->
	      let ll,hh =
          let a, b = B.floor e, B.ceil e in
          if B.equal a b then a, B.add_up b B.one
	        else a, b
        in
	      let res = (!to_pair,ll) in to_pair := hh ; res)
	      m
    in
    List.rev_map check_bot ((!to_pair,h)::list)

  let prune (((l,h) as a):t) (((l',h') as b):t) : t list * t  =
    if subseteq a b then [],a else
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
  let div (i1:t) (i2:t) : t bot * bool =
    (* split into positive and negative dividends *)
    let pos = (lift_bot (div_sign i1)) (meet i2 positive)
    and neg = (lift_bot (div_sign i1)) (meet i2 negative) in
    (* joins the result *)
    join_bot2 join pos neg,
    contains_float i2 0.

  (* interval square root *)
  let sqrt ((l,h):t) : t bot =
    if B.sign h < 0 then Bot else
    let l = B.max l B.zero in
    Nb (B.sqrt_down l, B.sqrt_up h)

  (* useful operators on intervals *)
  let ( +@ ) = add
  let ( -@ ) = sub
  let ( *@ ) = mul
  let ( /@ ) = div

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
    | Nb (l', h') -> fst (div (l',h') (of_bound ln10))

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
      | _ -> failwith "cant handle negatives powers"
    else failwith  "cant handle non_singleton powers"

  (* nth-root *)
  let n_root ((il,ih):t) ((l,h):t) =
    if B.equal l h && B.floor l = l then
      let p = B.to_float_down l |> int_of_float in
      match p with
      | 1 -> Nb (il, ih)
      | x when x > 1 && B.odd l ->
	Nb (B.root_down il p, B.root_up ih p)
      | x when x > 1 && B.even l ->
        if B.lt ih B.zero then Bot
        else if B.leq il B.zero then Nb (B.neg (B.root_up ih p), B.root_up ih p)
        else
          Nb (B.min (B.neg (B.root_down il p)) (B.neg (B.root_down ih p)), B.max (B.root_up il p) (B.root_up ih p))
      | _ -> failwith "can only handle stricly positive roots"
    else failwith  "cant handle non_singleton roots"


  (* interval min *)
  let min ((l1, u1):t) ((l2, u2):t) =
    validate (B.min l1 l2, B.min u1 u2)

  (* interval max *)
  let max ((l1, u1):t) ((l2, u2):t) =
    validate (B.max l1 l2, B.max u1 u2)

  (** runtime functions **)
  let eval_fun name args : t bot =
    let arity_1_bot (f: t -> t bot) : t bot =
       match args with
       | [i] ->
          (match f i with
          | Bot -> Bot
          | Nb i -> Nb i)
      | _ -> failwith (Format.sprintf "%s expect one argument" name)
    in
    let arity_2 (f: t -> t -> t) : t bot  =
      match args with
      | [i1;i2] -> Nb (f i1 i2)
      | _ -> failwith (Format.sprintf "%s expect two arguments" name)
    in
    let arity_2_bot (f: t -> t -> t bot) : t bot  =
      match args with
      | [i1;i2] ->
         (match f i1 i2 with
          | Bot -> Bot
          | Nb(i) -> Nb i)
      | _ -> failwith (Format.sprintf "%s expect two arguments" name)
    in
    match name with
    | "sqrt"  -> arity_1_bot sqrt
    | "max"   -> arity_2 max
    | "nroot" -> arity_2_bot n_root
    | s -> failwith (Format.sprintf "unknown eval function : %s" s)



  (************************************************************************)
  (* FILTERING (TEST TRANSFER FUNCTIONS) *)
  (************************************************************************)


  (* tests *)
  (* ----- *)

  let filter_leq ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
    merge_bot2 (check_bot (l1, B.min h1 h2)) (check_bot (B.max l1 l2, h2))

  let filter_geq ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
    merge_bot2 (check_bot (B.max l1 l2, h1)) (check_bot (l2, B.min h1 h2))

  let filter_lt ((l1,_) as i1:t) ((l2,h2) as i2:t) : (t*t) bot =
    if is_singleton i1 && is_singleton i2 && B.equal l1 l2 then Bot
    else if B.leq h2 l1 then Bot
    else filter_leq i1 i2

  let filter_gt ((l1,h1) as i1:t) ((l2,_) as i2:t) : (t*t) bot =
    if is_singleton i1 && is_singleton i2 && B.equal l1 l2 then Bot
    else if B.leq h1 l2 then Bot
    else filter_geq i1 i2

  let filter_eq (i1:t) (i2:t) : (t*t) bot =
    lift_bot (fun x -> x,x) (meet i1 i2)

  let filter_neq ((l1,_) as i1:t) ((l2,_) as i2:t) : (t*t) bot =
    if is_singleton i1 && is_singleton i2 && B.equal l1 l2 then Bot
    else Nb (i1,i2)

  let filter_lt_int ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
    merge_bot2
      (check_bot (l1, B.min h1 (B.sub_up h2 B.one)))
      (check_bot (B.max (B.add_down l1 B.one) l2, h2))

  let filter_gt_int ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
    merge_bot2
      (check_bot (B.max l1 (B.add_down l2 B.one), h1))
      (check_bot (l2, B.min (B.sub_up h1 B.one) h2))

  let filter_neq_int ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
    match is_singleton (l1,h1), is_singleton (l2,h2) with
    | true, true when B.equal l1 l2 -> Bot
    | true, false when B.equal l1 l2 ->
        merge_bot2 (Nb (l1,l2)) (check_bot (B.add_down l2 B.one, h2))
    | true, false when B.equal l1 h2 ->
        merge_bot2 (Nb (l1,l2)) (check_bot (l2, B.sub_up h2 B.one))
    | false, true when B.equal l1 l2 ->
        merge_bot2 (check_bot (B.add_down l1 B.one, h1)) (Nb (l2,h2))
    | false, true when B.equal h1 l2 ->
        merge_bot2 (check_bot (l1, B.sub_up h1 B.one)) (Nb (l2,h2))
    | _ -> Nb ((l1,h1),(l2,h2))


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
      else match fst (div r i2) with Bot -> Bot | Nb x -> meet i1 x)
      (if contains_float r 0. && contains_float i1 0. then Nb i2
      else match fst (div r i1) with Bot -> Bot | Nb x -> meet i2 x)

  (* r = i1/i2 => i1 = i2*r /\ (i2 = i1/r \/ i1=r=0) *)
  let filter_div (i1:t) (i2:t) (r:t) : (t*t) bot =
    merge_bot2
      (meet i1 (mul i2 r))
      (if contains_float r 0. && contains_float i1 0. then Nb i2
      else match fst (div i1 r) with Bot -> Bot | Nb x -> meet i2 x)

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
  let filter_log i r = failwith "todo filter_log"

  (* r = i ** n => i = nroot r *)
  let filter_pow (i:t) n (r:t) =
    merge_bot2 (meet_bot meet i (n_root r n)) (Nb n)

  (* r = nroot i => i = r ** n *)
  let filter_root i r n =
     merge_bot2 (meet i (pow r n)) (Nb n)

  (* r = min (i1, i2) *)
  let filter_min (l1, u1) (l2, u2) (lr, ur) =
    merge_bot2 (check_bot ((B.max l1 lr), (B.max u1 ur))) (check_bot ((B.max l2 lr), (B.max u2 ur)))

  (* r = max (i1, i2) *)
  let filter_max (l1, u1) (l2, u2) (lr, ur) =
    merge_bot2 (check_bot ((B.min l1 lr), (B.min u1 ur))) (check_bot ((B.min l2 lr), (B.min u2 ur)))

  let filter_fun name args r : (t list) bot =
    let arity_1 (f: t -> t -> t bot) : (t list) bot =
      match args with
      | [i] ->
         (match f i r with
         | Bot -> Bot
         | Nb i -> Nb [i])
      | _ -> failwith (Format.sprintf "%s expect one argument" name)
    in
    let arity_2 (f: t -> t -> t -> (t*t) bot) : (t list) bot  =
      match args with
      | [i1;i2] ->
         (match f i1 i2 r with
          | Bot -> Bot
          | Nb(i1,i2) -> Nb[i1;i2])
      | _ -> failwith (Format.sprintf "%s expect two arguments" name)
    in
    match name with
    | "sqrt" -> arity_1 filter_sqrt
    | "exp"  -> arity_1 filter_exp
    | "ln"   -> arity_1 filter_ln
    | "max"  -> arity_2 filter_max
    | "min"  -> arity_2 filter_min
    | s -> failwith (Format.sprintf "unknown filter function : %s" s)

  let filter_bounds (l,h) =
    let inf = B.ceil l
    and sup = B.floor h in
    check_bot (inf, sup)

  let to_float_range (l,h) =
    (B.to_float_down l),(B.to_float_up h)


end

module ItvF = Itv(Bound_float)
(* module ItvQ = Itv(Bound_rational) *)
