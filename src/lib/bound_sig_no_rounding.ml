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
  val of_int : int -> t
  val of_float : float -> t

  (* printing *)
  (* ******** *)
  val to_string: t -> string
  val print: Format.formatter -> t -> unit

  (* conversion *)
  (* ********** *)

  val to_float : t -> float
  val to_int   : t -> int

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

  val neg  : t -> t
  val abs  : t -> t
  val add  : t -> t -> t
  val sub  : t -> t -> t
  val mul  : t -> t -> t
  val div  : t -> t -> t
end
