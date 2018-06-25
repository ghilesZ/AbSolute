open Bot

type t = Int of Itv_int.t | Real of Itv.ItvF.t

(************************************************************************)
(* CONSTRUCTORS AND CONSTANTS *)
(************************************************************************)

let of_ints (x1:int) (x2:int) : t =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'of_ints' in file 'itv_mix.ml' not implemented"

let of_floats (x1:float) (x2:float) : t =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'of_floats' in file 'itv_mix.ml' not implemented"
(* [a,b] *)

let of_int (x1:int) : t =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'of_int' in file 'itv_mix.ml' not implemented"

let of_float (x1:float) : t =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'of_float' in file 'itv_mix.ml' not implemented"
(* {a} *)

(* maps empty intervals to explicit bottom *)
let check_bot (x1:t) : t bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'check_bot' in file 'itv_mix.ml' not implemented"

(************************************************************************)
(* PRINTING and CONVERSIONS *)
(************************************************************************)

let to_float_range (x1:t) : float * float =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'to_float_range' in file 'itv_mix.ml' not implemented"

let print (fmt:Format.formatter) (x3:t) : unit =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'print' in file 'itv_mix.ml' not implemented"

(************************************************************************)
(* SET-THEORETIC *)
(************************************************************************)

(* operations *)
(* ---------- *)

let join (x1:t) (x2:t) : t =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'join' in file 'itv_mix.ml' not implemented"

let meet (x1:t) (x2:t) : t bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'meet' in file 'itv_mix.ml' not implemented"

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

let is_singleton (x1:t) : bool =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'is_singleton' in file 'itv_mix.ml' not implemented"

(* mesure *)
(* ------ *)
let range (x1:t) : float =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'range' in file 'itv_mix.ml' not implemented"

(* split *)
(* ----- *)
let split (x1:t) : (t bot) list =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'split' in file 'itv_mix.ml' not implemented"

(* pruning *)
(* ------- *)
let prune (x1:t) (x2:t) : t list * t =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'prune' in file 'itv_mix.ml' not implemented"

(************************************************************************)
(* INTERVAL ARITHMETICS (FORWARD EVALUATION) *)
(************************************************************************)

let neg (x1:t) : t =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'neg' in file 'itv_mix.ml' not implemented"

let abs (x1:t) : t =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'abs' in file 'itv_mix.ml' not implemented"

let add (x1:t) (x2:t) : t =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'add' in file 'itv_mix.ml' not implemented"

let sub (x1:t) (x2:t) : t =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'sub' in file 'itv_mix.ml' not implemented"

let mul (x1:t) (x2:t) : t =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'mul' in file 'itv_mix.ml' not implemented"

(* return valid values (possibly Bot) + possible division by zero *)
let div (x1:t) (x2:t) : t bot * bool =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'div' in file 'itv_mix.ml' not implemented"

(* returns valid value when the exponant is a singleton positive integer.
     fails otherwise
 *)
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
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_leq' in file 'itv_mix.ml' not implemented"

let filter_geq (x1:t) (x2:t) : (t * t) bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_geq' in file 'itv_mix.ml' not implemented"

let filter_lt (x1:t) (x2:t) : (t * t) bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_lt' in file 'itv_mix.ml' not implemented"

let filter_gt (x1:t) (x2:t) : (t * t) bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_gt' in file 'itv_mix.ml' not implemented"

let filter_eq (x1:t) (x2:t) : (t * t) bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_eq' in file 'itv_mix.ml' not implemented"

let filter_neq (x1:t) (x2:t) : (t * t) bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_neq' in file 'itv_mix.ml' not implemented"

(* given the interval argument(s) and the expected interval result of
     a numeric operation, returns a refined interval argument(s) where
     points that cannot contribute to a value in the result are
     removed;
     may also return Bot if no point in an argument can lead to a
     point in the result *)

let filter_neg (x1:t) (x2:t) : t bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_neg' in file 'itv_mix.ml' not implemented"

let filter_abs (x1:t) (x2:t) : t bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_abs' in file 'itv_mix.ml' not implemented"

let filter_add (x1:t) (x2:t) (x3:t) : (t*t) bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_add' in file 'itv_mix.ml' not implemented"

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
     it returns a possibly bottom result
 *)
let filter_fun (x1:string) (x2:t list) (x3:t) : (t list) bot =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'filter_fun' in file 'itv_mix.ml' not implemented"

(* generate a random float within the given interval *)
let spawn (x1:t) : float =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'spawn' in file 'itv_mix.ml' not implemented"
