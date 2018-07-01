(*
   Generic intervals.

   Can be instantiated with any bound type.
*)


open Bot
open Bound_sig_simple

module Itv(B:BOUND) = struct

  (************************************************************************)
  (* TYPES *)
  (************************************************************************)

  (* interval bound (possibly -oo or +oo *)
  type bound = B.t

  (* an interval is a pair of bounds (lower,upper);
     intervals are always non-empty: lower <= upper;
     functions that can return an empty interval return it as Bot
   *)
  type t = bound * bound

  (* maps empty intervals to explicit bottom *)
  let check_bot ((l,h):t) : t bot =
    if B.leq l h then Nb (l,h) else Bot

  (************************************************************************)
  (* CONSTRUCTORS AND CONSTANTS *)
  (************************************************************************)

  let of_bound (x:B.t) : t = (x,x)

  let zero : t = of_bound B.zero

  let one : t = of_bound B.one

  let minus_one : t = of_bound B.minus_one

  let zero_one : t = B.zero, B.one

  let positive : t = B.zero, B.inf

  let negative : t = B.minus_inf, B.zero

  let of_bounds (l:bound) (h:bound) = (l,h)

  let of_ints (l:int) (h:int) : t = of_bounds (B.of_int l) (B.of_int h)

  let of_int (x:int) = of_ints x x

  let of_floats (l:float) (h:float) : t = of_bounds (B.of_float l) (B.of_float h)

  let of_float (x:float) = of_floats x x

  let float_hull (x:float) (y:float) = min x y, max x y

  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)

  (* printing *)
  let print fmt ((l,h):t) =
    Format.fprintf fmt "[%a;%a]" B.print l B.print h


  (************************************************************************)
  (* SET-THEORETIC *)
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
    B.leq l (B.of_float x) && B.leq (B.of_float x) h

  let intersect ((l1,h1):t) ((l2,h2):t) : bool =
    B.leq l1 h2 && B.leq l2 h1

  let is_singleton ((l,h):t) : bool = l=h

  (* mesure *)
  (* ------ *)

  (* length of the intersection (>= 0) *)
  let overlap ((l1,h1):t) ((l2,h2):t) : B.t  =
    B.max B.zero (B.sub (B.min h1 h2) (B.max l1 l2))

  let range ((l,h):t) : B.t = B.sub h l

  (* split *)
  (* ----- *)

  (* find the mean of the interval;
     when a bound is infinite, then "mean" is some value strictly inside the
     interval
   *)
  let mean ((l,h):t) : B.t list = [B.div (B.add l h) B.two]

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
          let a = B.to_int e |> B.of_int in
          a, B.add e B.one
        in
	      let res = (!to_pair,ll) in to_pair := hh ; res)
	      m
    in
    List.rev_map check_bot ((!to_pair,h)::list)

  let prune a b : t list * t  =
    failwith "no pruning available for now with itv_no_rounding"

  (*********************************************)
  (* INTERVAL ARITHMETICS (FORWARD EVALUATION) *)
  (*********************************************)

  let neg ((l,h):t) : t =
    B.neg h, B.neg l

  let abs ((l,h):t) : t =
    if contains_float (l,h) 0. then B.zero, B.max (B.abs l) (B.abs h)
    else if B.gt (B.abs l) (B.abs h) then of_bounds (B.abs h) (B.abs l)
    else of_bounds (B.abs l) (B.abs h)

  let add ((l1,h1):t) ((l2,h2):t) : t =
    B.add l1 l2, B.add h1 h2

  let sub ((l1,h1):t) ((l2,h2):t) : t =
    B.sub l1 h2, B.sub h1 l2

  let mix4 f ((l1,h1):t) ((l2,h2):t) =
    B.min (B.min (f l1 l2) (f l1 h2)) (B.min (f h1 l2) (f h1 h2)),
    B.max (B.max (f l1 l2) (f l1 h2)) (B.max (f h1 l2) (f h1 h2))

  let mul = mix4 B.mul

  (* helper: assumes i2 has constant sign *)
  let div_sign = mix4 B.div

  (* return valid values (possibly Bot) + possible division by zero *)
  let div (i1:t) (i2:t) : t bot * bool =
    (* split into positive and negative dividends *)
    let pos = (lift_bot (div_sign i1)) (meet i2 positive)
    and neg = (lift_bot (div_sign i1)) (meet i2 negative) in
    (* joins the result *)
    join_bot2 join pos neg,
    contains_float i2 0.

  (* powers *)
  let pow ((il,ih):t) ((l,h):t) =
    failwith "cant handle powers with itv no rounding"

  (* interval min *)
  let min ((l1, u1):t) ((l2, u2):t) =
    (B.min l1 l2, B.min u1 u2)

  (* interval max *)
  let max ((l1, u1):t) ((l2, u2):t) =
    (B.max l1 l2, B.max u1 u2)

  (** runtime functions **)
  let eval_fun name args : t bot =
    failwith (Format.sprintf "unknown eval function : %s" name)

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
      else match fst (div r i2) with Bot -> Bot | Nb x -> meet i1 x)
      (if contains_float r 0. && contains_float i1 0. then Nb i2
      else match fst (div r i1) with Bot -> Bot | Nb x -> meet i2 x)

  (* r = i1/i2 => i1 = i2*r /\ (i2 = i1/r \/ i1=r=0) *)
  let filter_div (i1:t) (i2:t) (r:t) : (t*t) bot =
    merge_bot2
      (meet i1 (mul i2 r))
      (if contains_float r 0. && contains_float i1 0. then Nb i2
      else match fst (div i1 r) with Bot -> Bot | Nb x -> meet i2 x)

  (* r = i ** n => i = nroot r *)
  let filter_pow (i:t) n (r:t) = failwith "filter pow"

  (* r = min (i1, i2) *)
  let filter_min (l1, u1) (l2, u2) (lr, ur) =
    merge_bot2 (check_bot ((B.max l1 lr), (B.max u1 ur))) (check_bot ((B.max l2 lr), (B.max u2 ur)))

  (* r = max (i1, i2) *)
  let filter_max (l1, u1) (l2, u2) (lr, ur) =
    merge_bot2 (check_bot ((B.min l1 lr), (B.min u1 ur))) (check_bot ((B.min l2 lr), (B.min u2 ur)))

  let filter_fun name args r : (t list) bot =
    failwith (Format.sprintf "unknown filter function : %s" name)

  let to_float_range (l,h) =
    (B.to_float l),(B.to_float h)
end
