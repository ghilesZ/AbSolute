open Bot

type bound = int

type t = bound * bound

(* not all pairs of integers are valid intervals *)
let validate ((l,h):t) : t =
  if l > h then
    invalid_arg
      ("itv_int.validate: "
       ^ string_of_int l
       ^" "
       ^string_of_int h)
  else l,h

(* maps empty intervals to explicit bottom *)
let check_bot ((l,h):t) : t bot =
  if l <= h then Nb (l,h) else Bot

(************************************************************************)
(* CONSTRUCTORS AND CONSTANTS *)
(************************************************************************)

let of_bound (x:bound) : t = validate (x,x)

let of_bounds (l:bound) (h:bound) = validate (l,h)

let of_ints = of_bounds

let of_floats a b : t bot =
  check_bot (of_ints (int_of_float (floor a)) (int_of_float (ceil b)))

let of_int = of_bound

(*No integer interval can exactly abstract a single float *)
let of_float = Bot

let positive : t = (1,max_int)
let negative : t = (min_int,-1)

(************************************************************************)
(* PRINTING and CONVERSIONS *)
(************************************************************************)

let to_float_range ((a,b):t) = (float a), (float b)

let print (fmt:Format.formatter) ((a,b):t) =
  Format.fprintf fmt "[%i,%i]" a b

(************************************************************************)
(* SET-THEORETIC *)
(************************************************************************)

(* operations *)
(* ---------- *)
let join (l1,h1:t) (l2,h2:t) : t = (min l1 l2), (max h1 h2)
let meet (l1,h1:t) (l2,h2:t) : t bot = check_bot ((max l1 l2), (min h1 h2))

(* predicates *)
(* ---------- *)
(* returns true if f can be conferted exactly to an integer *)
let contains_float ((a,b):t) f =
  let rounded = ceil f in
  rounded = f &&
    let rounded = int_of_float rounded in
    a <= rounded && rounded <= b

let intersect ((l1,h1):t) ((l2,h2):t) = l1 <= h2 &&  l2 <= h1

(* mesure *)
(* ------ *)
let range ((a,b):t) = b - a

(* split *)
(* ----- *)

(* splits in two, around the middle *)
let split ((a,b) :t) =
  assert (a<b);
  if a+1=b then [Nb (a,a); Nb(b,b)]
  else
    let mid = a + (b-a)/2 in
    [Nb (a,mid); Nb(mid+1,b)]

let prune (l1,u1:t) (l2,u2:t) : t list * t =
  match (l1 < l2),(u2 < u1) with
  | true , true  -> [(l1,(l2-1));((u2+1),u1)],(l2,u2)
  | true , false -> [(l1,(l2-1))],(l2,u1)
  | false, true  -> [((u2+1),u1)],(l1,u2)
  | false, false -> [],(l2,u2)

(************************************************************************)
(* INTERVAL ARITHMETICS (FORWARD EVALUATION) *)
(************************************************************************)

let neg (l,h:t) : t =
  -h, -l

let abs ((l,h) as i:t) : t =
  if l < 0 then 0,(max h (-l)) else i

let add (l1,h1:t) (l2,h2:t) : t =
  l1+l2, h1+h2

let sub (l1,h1:t) (l2,h2:t) : t =
  l1-h2, h1-l2

(* tries the different possibilities *)
let mix4 f l1 h1 l2 h2 =
  (min (min (f l1 l2) (f l1 h2)) (min (f h1 l2) (f h1 h2))),
  (max (max (f l1 l2) (f l1 h2)) (max (f h1 l2) (f h1 h2)))

let mul (l1,h1:t) (l2,h2:t) : t =
  mix4 ( * ) l1 h1 l2 h2

let div_sign (l1,h1) (l2,h2)  = mix4 ( / ) l1 h1 l2 h2

(* return valid values (possibly Bot) *)
let div (i1:t) (i2:t) : t bot =
  (* split into positive and negative dividends *)
  let pos = (lift_bot (div_sign i1)) (meet i2 positive)
  and neg = (lift_bot (div_sign i1)) (meet i2 negative) in
  (* joins the result *)
  join_bot2 join pos neg

(* returns valid value when the exponant is a singleton positive integer.
   fails otherwise *)
let pow =
  let pow_aux i exp = int_of_float ((float i) ** (float exp)) in
  fun (l1,u1:t) (l2,u2:t) ->
  if l2=u2 then
    let exp = l2 in
    match exp with
    | 0 -> (1,1)
    | 1 -> (l1, u1)
    | x when x > 1 && exp mod 2 = 1 -> (pow_aux l1 exp),(pow_aux u1 exp)
    | x when x > 1 && exp mod 2 = 0 ->
       if l1 >= 0 then
	       (pow_aux exp l2),(pow_aux u1 exp)
       else if u1 <= 0 then
         (pow_aux u1 exp),(pow_aux l1 exp)
       else
         0,max (pow_aux l1 exp) (pow_aux u1 exp)
    | _ -> failwith "cant handle negatives powers"
  else failwith  "itv_int.ml cant handle non_singleton powers"

(* function calls (sqrt, exp, ln ...) are handled here :
   given a function name and and a list of argument,
   it returns a possibly bottom result *)
let eval_fun (x1:string) (x2:t list) : t bot =
  (*TODO: replace "assert false" with your own code *)
  assert false

(************************************************************************)
(* FILTERING (TEST TRANSFER FUNCTIONS)                                  *)
(************************************************************************)
let filter_leq (l1,h1:t) (l2,h2:t) : (t * t) bot =
  merge_bot2 (check_bot (l1, min h1 h2)) (check_bot (max l1 l2, h2))

let filter_lt ((l1,h1) as i1:t) ((l2,h2) as i2:t) : (t * t) bot =
  if l1 = h1 && l2 = h2 && l1 = l2 then Bot
  else if h2 <= l1 then Bot
  else filter_leq i1 i2

let filter_eq (i1:t) (i2:t) : (t * t) bot =
  lift_bot (fun x -> x,x) (meet i1 i2)

let filter_neq ((l1,h1) as i1:t) ((l2,h2) as i2:t) : (t * t) bot =
  if l1=h1 && l2=h2 && l1 = l2 then Bot
  else Nb (i1,i2)

(* arithmetic *)
(* --------- *)

(* r = -i => i = -r *)
let filter_neg (i:t) (r:t) : t bot =
  meet i (neg r)

let filter_abs ((il,ih) as i:t) ((rl,rh) as r:t) : t bot =
  if il >= 0 then meet i r
  else if ih <= 0 then meet i (neg r)
  else meet i (-rh, rh)

let filter_add (i1:t) (i2:t) (r:t) : (t*t) bot =
  merge_bot2 (meet i1 (sub r i2)) (meet i2 (sub r i1))

let filter_sub (i1:t) (i2:t) (r:t) : (t*t) bot =
  merge_bot2 (meet i1 (add i2 r)) (meet i2 (sub i1 r))

let filter_mul (i1:t) (i2:t) (r:t) : (t*t) bot =
  merge_bot2
    (if contains_float r 0. && contains_float i2 0. then Nb i1
     else
       match div r i2
       with
       | Bot -> Bot
       | Nb x -> meet i1 x)
    (if contains_float r 0. && contains_float i1 0. then Nb i2
     else
       match div r i1
       with
       | Bot -> Bot
       | Nb x -> meet i2 x)

let filter_div (x1:t) (x2:t) (x3:t) : (t*t) bot =
  (*TODO: replace "assert false" with your own code *)
  assert false

let filter_pow (x1:t) (x2:t) (x3:t) : (t*t) bot =
  (*TODO: replace "assert false" with your own code *)
  assert false

(* filtering function calls like (sqrt, exp, ln ...) is done here :
     given a function name, a list of argument, and a result,
     it remove points that cannot satisfy the relation : f(arg1,..,argn) = r;
     it returns a possibly bottom result *)
let filter_fun (x1:string) (x2:t list) (x3:t) : (t list) bot =
  (*TODO: replace "assert false" with your own code *)
  assert false

(* generate a random float within the given interval *)
let spawn (l,h:t) : int =
  let r = Random.int ((h-l)+1) in
  l + r
