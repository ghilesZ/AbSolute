(* Signature for bounds: numeric values enriched with +oo and -oo.
 *)


module type BOUND = sig

  type t


  (* ordering *)
  (* ******** *)

  val compare: t -> t -> int

  val equal: t -> t -> bool
  val leq: t -> t -> bool
  val geq: t -> t -> bool
  val lt: t -> t -> bool
  val gt: t -> t -> bool
  val neq: t -> t -> bool

  val odd: t -> bool
  val even: t -> bool

  val min: t -> t -> t
  val max: t -> t -> t

  val sign: t -> int


  (* construction *)
  (* ************ *)

  (* operators and conversions are tagged with a _up or _down suffix
     to indicate rounding direction
   *)

  val of_int_up: int -> t
  val of_int_down: int -> t
  val of_float_up: float -> t
  val of_float_down: float -> t

  (* printing *)
  (* ******** *)
  val print: Format.formatter -> t -> unit

  (* conversion *)
  (* ********** *)
  val to_float_up: t -> float
  val to_float_down: t -> float

  (* useful constants *)
  (* **************** *)
  val zero: t
  val one: t
  val two: t
  val minus_one: t
  val inf: t
  val minus_inf: t

  (* operators *)
  (* ********* *)

  (* exact operators *)
  val neg: t -> t
  val abs: t -> t

  (* operators with rounding *)
  val add_up: t -> t -> t
  val sub_up: t -> t -> t
  val mul_up: t -> t -> t
  val div_up: t -> t -> t
  val add_down: t -> t -> t
  val sub_down: t -> t -> t
  val mul_down: t -> t -> t
  val div_down: t -> t -> t

  val bound_mul: (t -> t -> t) -> t -> t -> t
  val bound_div: (t -> t -> t) -> t -> t -> t

  val sqrt_up: t -> t
  val sqrt_down: t -> t

  val pow_up: t -> int -> t
  val pow_down: t -> int -> t
  val root_up: t -> int -> t
  val root_down: t -> int -> t

  val exp_up: t -> t
  val exp_down: t -> t
  val ln_up: t -> t
  val ln_down: t -> t
  val log_up: t -> t
  val log_down: t -> t

  (* integer rounding *)
  val floor: t -> t
  val ceil: t -> t

end
