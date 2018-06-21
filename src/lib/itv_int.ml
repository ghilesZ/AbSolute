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
let join a b = assert false
let meet a b = assert false

(* predicates *)
(* ---------- *)
let subseteq ((a,b):t) ((c,d):t) = a <= c && d <= b

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
  if a+1 = b then [Some (a,a); Some(b,b)]
  else
    let mid = mean i in
    [Some (a,mid); Some(mid,b)]

let prune (((l,h) as a):t) (((l',h') as b):t) = assert false


(************************************************************************)
(* INTERVAL ARITHMETICS (FORWARD EVALUATION) *)
(************************************************************************)

let neg ((l,h):t) : t =
  -h, -l

let abs ((l,h) as i:t) : t =
  if l < 0 then 0,(max h (-l)) else i

let add ((l1,h1):t) ((l2,h2):t) : t =
  l1 + l2, h1 + h2

let sub ((l1,h1):t) ((l2,h2):t) : t =
  l1 - h2, h1 - l2

let mul ((l1,h1):t) ((l2,h2):t) : t =
  assert false
