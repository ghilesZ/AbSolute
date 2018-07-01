open Bot

type bound = int

type t = bound * bound

(* not all pairs of integers are valid intervals *)
let validate ((l,h):t) : t =
  if l > h then
    invalid_arg
      ("int.validate: "
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
let of_floats a b = of_ints (int_of_float (floor a)) (int_of_float (ceil b))

let of_int = of_bound

(*No integer interval can exactly abstract a single float *)
let of_float = Bot

(************************************************************************)
(* PRINTING and CONVERSIONS *)
(************************************************************************)

let to_float_range ((a,b):t) = (float a),(float b)

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

let contains_float ((a,b):t) f = assert false

let intersect ((l1,h1):t) ((l2,h2):t) = l1 <= h2 &&  l2 <= h1

let is_singleton ((a,b):t) = a = b

(* mesure *)
(* ------ *)
let range ((a,b):t) = b - a

(* split *)
(* ----- *)

(* finds the mean of the interval *)
let mean ((a,b):t) = a + (b-a)/2

(* splits in two, around the middle *)
let split ((a,b) as i :t) =
  if a+1 = b then [Nb (a,a); Nb(b,b)]
  else
    let mid = mean i in
    [Nb (a,mid); Nb(mid,b)]

let prune (x1:t) (x2:t) : t list * t =
  (*TODO: replace "assert false" with your own code *)
  assert false

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
  let ll = f l1 l2
  and lh = f l1 h2
  and hl = f h1 l2
  and hh = f h1 h2
  in (min (min lh ll) (min hl hh)), (max (max lh ll) (max hl hh))

let mul (l1,h1:t) (l2,h2:t) : t =
  mix4 ( * ) l1 h1 l2 h2

(* return valid values (possibly Bot) + possible division by zero *)
let div (l1,h1:t) (l2,h2:t) : t bot * bool =
  if l2=h2 && l2=0 then (Bot,true)
  else
    if l2 <= 0 && 0 <= h2 then
      if l2 = 0 then Nb (mix4 ( / ) l1 h1 1 h2),true
      else if h2 = 0 then Nb (mix4 ( / ) l1 h1 l2 (-1)),true
      else Nb (join (mix4 ( / ) l1 h1 l2 0) (mix4 ( / ) l1 h1 0 h2)),true
    else Nb (mix4 ( / ) l1 h1 l2 h2), false

(* returns valid value when the exponant is a singleton positive integer.
   fails otherwise *)
let pow (x1:t) (x2:t) : t =
  (*TODO: replace "assert false" with your own code *)
  assert false

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

let filter_mul (x1:t) (x2:t) (x3:t) : (t*t) bot =
  (*TODO: replace "assert false" with your own code *)
  assert false

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
  let r = Random.int (h-l) in
  l + r
