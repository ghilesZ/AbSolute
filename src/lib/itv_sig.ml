(*
   Generic signature for intervals.
   The interface is functional.
 *)

open Bot

module type ITV = sig

  (************************************************************************)
  (* TYPES *)
  (************************************************************************)

  (* an interval is a pair of bounds (lower,upper);
     intervals are always non-empty: lower <= upper;
     functions that can return an empty interval return it as Bot *)
  type t (* = bound * bound *)

  (************************************************************************)
  (* CONSTRUCTORS AND CONSTANTS *)
  (************************************************************************)

  val of_ints : int -> int -> t
  val of_floats : float -> float -> t
  (* [a,b] *)

  val of_int : int -> t
  val of_float : float -> t
  (* {a} *)

  (* maps empty intervals to explicit bottom *)
  val check_bot: t -> t bot

  (************************************************************************)
  (* PRINTING and CONVERSIONS *)
  (************************************************************************)

  val to_float_range : t -> float * float

  val print : Format.formatter -> t -> unit

  (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)

  (* operations *)
  (* ---------- *)

  val join: t -> t -> t
  val meet: t -> t -> t bot

  (* predicates *)
  (* ---------- *)
  val subseteq: t -> t -> bool
  val contains_float: t -> float -> bool
  val intersect: t -> t -> bool
  val is_singleton: t -> bool

  (* mesure *)
  (* ------ *)
  val range: t -> float

  (* split *)
  (* ----- *)
  val split: t -> (t bot) list

  (* pruning *)
  (* ------- *)
  val prune : t -> t -> t list * t

  (************************************************************************)
  (* INTERVAL ARITHMETICS (FORWARD EVALUATION) *)
  (************************************************************************)

  val neg: t -> t
  val abs: t -> t

  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t

  (* return valid values (possibly Bot) + possible division by zero *)
  val div: t -> t -> t bot * bool

  (* returns valid value when the exponant is a singleton positive integer.
     fails otherwise
  *)
  val pow: t -> t -> t

  (* function calls (sqrt, exp, ln ...) are handled here :
     given a function name and and a list of argument,
     it returns a possibly bottom result
   *)
  val eval_fun : string -> t list -> t bot

  (************************************************************************)
  (* FILTERING (TEST TRANSFER FUNCTIONS) *)
  (************************************************************************)

  (* given two interval arguments, return a subset of each argument
     by removing points that cannot satisfy the predicate;
     may also return Bot if no point can satisfy the predicate
   *)

  val filter_leq: t -> t -> (t * t) bot
  val filter_geq: t -> t -> (t * t) bot
  val filter_lt: t -> t -> (t * t) bot
  val filter_gt: t -> t -> (t * t) bot
  val filter_eq: t -> t -> (t * t) bot
  val filter_neq: t -> t -> (t * t) bot

  (* given the interval argument(s) and the expected interval result of
     a numeric operation, returns a refined interval argument(s) where
     points that cannot contribute to a value in the result are
     removed;
     may also return Bot if no point in an argument can lead to a
     point in the result
   *)

  val filter_neg: t -> t -> t bot
  val filter_abs: t -> t -> t bot

  val filter_add: t -> t -> t -> (t*t) bot
  val filter_sub: t -> t -> t -> (t*t) bot
  val filter_mul: t -> t -> t -> (t*t) bot
  val filter_div: t -> t -> t -> (t*t) bot

  val filter_pow: t -> t -> t -> (t*t) bot

  (* filtering function calls like (sqrt, exp, ln ...) is done here :
     given a function name, a list of argument, and a result,
     it remove points that cannot satisfy the relation : f(arg1,..,argn) = r;
     it returns a possibly bottom result
   *)
  val filter_fun: string -> t list -> t -> (t list) bot

  (* bound tightening over Integers values *)
  val filter_bounds: t -> t bot

  (* generate a random float within the given interval *)
  val spawn : t -> float

end
