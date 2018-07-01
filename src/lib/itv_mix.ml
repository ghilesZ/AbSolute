open Bot

module I = Itv_int
module F = Itv.ItvF

type t = Int of I.t | Real of F.t

let to_float (i:I.t) =
  let (a,b) = I.to_float_range i in
  F.of_floats a b

let to_int (r:F.t) =
  let (a,b) = F.to_float_range r in
  I.of_floats (ceil a) (floor b)

let dispatch f_int f_real = function
  | Int x -> f_int x
  | Real x -> f_real x

let map f_int f_real = function
  | Int x -> Int (f_int x)
  | Real x -> Real (f_real x)

(************************************************************************)
(* CONSTRUCTORS AND CONSTANTS *)
(************************************************************************)

let of_ints (x1:int) (x2:int) : t =
  Int(I.of_ints x1 x2)

let of_floats (x1:float) (x2:float) : t =
  Real(F.of_floats x1 x2)

let of_int (x1:int) : t =
  of_ints x1 x1

let of_float (x1:float) : t =
  of_floats x1 x1

(* maps empty intervals to explicit bottom *)
let check_bot (x:t) : t bot =
  match x with
  | Int x -> lift_bot (fun x -> Int x) (I.check_bot x)
  | Real x -> lift_bot (fun x -> Real x) (F.check_bot x)

(************************************************************************)
(* PRINTING and CONVERSIONS *)
(************************************************************************)

let to_float_range (x:t) : float * float =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'to_float_range' in file 'itv_mix.ml' not implemented"

let print (fmt:Format.formatter) (x:t) : unit =
  dispatch (Format.fprintf fmt "%a" I.print) (Format.fprintf fmt "%a" F.print) x

(************************************************************************)
(* SET-THEORETIC *)
(************************************************************************)

(* operations *)
(* ---------- *)

let join (x1:t) (x2:t) : t =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'join' in file 'itv_mix.ml' not implemented"

let meet (x1:t) (x2:t) : t bot =
  match x1,x2 with
  | Int x1, Int x2 -> lift_bot (fun x -> Int x) (I.meet x1 x2)
  | Real x1, Real x2 -> lift_bot (fun x -> Real x) (F.meet x1 x2)
  | Int x1, Real x2 | Real x2, Int x1 -> lift_bot (fun x -> Int x) (I.meet x1 (to_int x2))

(* predicates *)
(* ---------- *)
let subseteq (x1:t) (x2:t) : bool =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'subseteq' in file 'itv_mix.ml' not implemented"

let contains_float (x1:t) (x2:float) : bool =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'contains_float' in file 'itv_mix.ml' not implemented"

let intersect (x1:t) (x2:t) : bool =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'intersect' in file 'itv_mix.ml' not implemented"

let is_singleton (x:t) : bool = dispatch I.is_singleton F.is_singleton x

(* mesure *)
(* ------ *)
let range (x:t) : float =
  match x with
  | Int x -> float (I.range x)
  | Real x -> F.range x

(* split *)
(* ----- *)
let split (x:t) : t bot list =
  match x with
  | Real x -> F.split x |> List.map (lift_bot (fun x -> Real x))
  | Int x -> I.split x  |> List.map (lift_bot (fun x -> Int x))

(* pruning *)
(* ------- *)
let prune (x1:t) (x2:t) : t list * t =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'prune' in file 'itv_mix.ml' not implemented"

(************************************************************************)
(* INTERVAL ARITHMETICS (FORWARD EVALUATION) *)
(************************************************************************)

let neg (x:t) : t = map I.neg F.neg x

let abs (x:t) : t = map I.abs F.abs x

let add (x1:t) (x2:t) : t =
  match x1,x2 with
  | Int x1 , Int x2 ->  Int(I.add x1 x2)
  | Real x1, Real x2 -> Real (F.add x1 x2)
  | Int x1, Real x2 | Real x2, Int x1 -> Real (F.add x2 (to_float x1))

let sub (x1:t) (x2:t) : t =
  match x1,x2 with
  | Int x1 , Int x2 ->  Int(I.sub x1 x2)
  | Real x1, Real x2 -> Real (F.sub x1 x2)
  | Int x1, Real x2 | Real x2, Int x1 -> Real (F.sub x2 (to_float x1))

let mul (x1:t) (x2:t) : t =
  match x1,x2 with
  | Int x1 , Int x2 ->  Int(I.mul x1 x2)
  | Real x1, Real x2 -> Real (F.mul x1 x2)
  | Int x1, Real x2 | Real x2, Int x1 -> Real (F.mul x2 (to_float x1))

(* return valid values (possibly Bot) + possible division by zero *)
let div (x1:t) (x2:t) : t bot * bool =
  match x1,x2 with
  | Int x1 , Int x2 ->  (* Int(I.div x1 x2) *) assert false
  | Real x1, Real x2 -> (* Real (F.div x1 x2) *) assert false
  | Int x1, Real x2 | Real x2, Int x1 -> (* Real (F.mul x2 (to_float x1)) *) assert false

(* returns valid value when the exponant is a singleton positive integer.
     fails otherwise *)
let pow (x1:t) (x2:t) : t =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'pow' in file 'itv_mix.ml' not implemented"

(* function calls (sqrt, exp, ln ...) are handled here :
     given a function name and and a list of argument,
     it returns a possibly bottom result
 *)
let eval_fun (x1:string) (x2:t list) : t bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'eval_fun' in file 'itv_mix.ml' not implemented"

(************************************************************************)
(* FILTERING (TEST TRANSFER FUNCTIONS) *)
(************************************************************************)

(* given two interval arguments, return a subset of each argument
   by removing points that cannot satisfy the predicate;
   may also return Bot if no point can satisfy the predicate *)

let filter_leq (x1:t) (x2:t) : (t * t) bot =
  match x1,x2 with
  | Int x1 , Int x2 ->  lift_bot (fun (x,y) -> (Int x),(Int y)) (I.filter_leq x1 x2)
  | Real x1, Real x2 -> lift_bot (fun (x,y) -> (Real x),(Real y)) (F.filter_leq x1 x2)
  | Int x1, Real x2 ->
     let x1 = to_float x1 in
     lift_bot (fun (x1,x2) -> Int (to_int x1), Real x2) (F.filter_leq x1 x2)
  | Real x1, Int x2 ->
     let x2 = to_float x2 in
     lift_bot (fun (x1,x2) -> Real x1, Int (to_int x2)) (F.filter_leq x1 x2)

let filter_lt (x1:t) (x2:t) : (t * t) bot =
  match x1,x2 with
  | Int x1 , Int x2 ->  lift_bot (fun (x,y) -> (Int x),(Int y)) (I.filter_lt x1 x2)
  | Real x1, Real x2 -> lift_bot (fun (x,y) -> (Real x),(Real y)) (F.filter_lt x1 x2)
  | Int x1, Real x2 ->
     let x1 = to_float x1 in
     lift_bot (fun (x1,x2) -> Int (to_int x1), Real x2) (F.filter_lt x1 x2)
  | Real x1, Int x2 ->
     let x2 = to_float x2 in
     lift_bot (fun (x1,x2) -> Real x1, Int (to_int x2)) (F.filter_lt x1 x2)

let filter_eq (x1:t) (x2:t) : (t * t) bot =
  match x1,x2 with
  | Int x1 , Int x2 ->  lift_bot (fun (x,y) -> (Int x),(Int y)) (I.filter_eq x1 x2)
  | Real x1, Real x2 -> lift_bot (fun (x,y) -> (Real x),(Real y)) (F.filter_eq x1 x2)
  | Int x1, Real x2 | Real x2, Int x1 ->
     let x2 = to_int x2 in
     lift_bot (fun (x1,x2) -> Int x1, Int x2) (I.filter_eq x1 x2)

let filter_neq (i1:t) (i2:t) : (t * t) bot =
  match i1,i2 with
  | Int x1 , Int x2 ->  lift_bot (fun (x,y) -> (Int x),(Int y)) (I.filter_neq x1 x2)
  | Real x1, Real x2 -> lift_bot (fun (x,y) -> (Real x),(Real y)) (F.filter_neq x1 x2)
  | Int x1, Real x2 | Real x2, Int x1 ->
     failwith "function 'filter_neq' in file 'itv_mix.ml', case INTxREAL not implented"

(* given the interval argument(s) and the expected interval result of
   a numeric operation, returns a refined interval argument(s) where
   points that cannot contribute to a value in the result are
   removed;
   may also return Bot if no point in an argument can lead to a
   point in the result *)

let filter_neg (x1:t) (x2:t) : t bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_neg' in file 'itv_mix.ml' not implemented"

(* r = i1+i2 => i1 = r-i2 /\ i2 = r-i1 *)
let filter_abs (x1:t) (x2:t) : t bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_abs' in file 'itv_mix.ml' not implemented"

let filter_add (i1:t) (i2:t) (r:t) : (t*t) bot =
  merge_bot2 (meet i1 (sub r i2)) (meet i2 (sub r i1))

let filter_sub (x1:t) (x2:t) (x3:t) : (t*t) bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_sub' in file 'itv_mix.ml' not implemented"

let filter_mul (x1:t) (x2:t) (x3:t) : (t*t) bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_mul' in file 'itv_mix.ml' not implemented"

let filter_div (x1:t) (x2:t) (x3:t) : (t*t) bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_div' in file 'itv_mix.ml' not implemented"

let filter_pow (x1:t) (x2:t) (x3:t) : (t*t) bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_pow' in file 'itv_mix.ml' not implemented"

(* filtering function calls like (sqrt, exp, ln ...) is done here :
     given a function name, a list of argument, and a result,
     it remove points that cannot satisfy the relation : f(arg1,..,argn) = r;
     it returns a possibly bottom result *)
let filter_fun (x1:string) (x2:t list) (x3:t) : (t list) bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_fun' in file 'itv_mix.ml' not implemented"

(* generate a random float within the given interval *)
let spawn (x:t) : float =
  match x with
  | Int x  -> float (I.spawn x)
  | Real x -> F.spawn x
