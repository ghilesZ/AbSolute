(**********************************************************************************)
(*            Module for Abstract Domains for Constraint Programming.             *)
(*   These are abstract domains with consistency, split and precision operators.  *)
(**********************************************************************************)

module type AbstractCP = sig

  (*** TYPES ***)
  (* abstract element : search space + constraints *)
  (* this type allows a custom representation of the constraints + variables *)
  type t
  type frontier

  type consistency =
    | Empty
    | Full of t
    | Maybe of frontier * t

  (*** Initialization ***)
  val init : Csp.prog -> t

  (*** termination: tests if an abstract element is too small to be cut *)
  val is_small : t -> bool

  (*** volume *)
  val volume : t -> float

  (*** OPERATIONS ***)

  (* removes inconsistent values from the search space *)
  val consistency : t -> consistency

  (* splits an abstract element *)
  val exploration : t -> t list

  (* pruning *)
  val prune : t -> frontier -> t list * t list

  (* printing *)
  val print : Format.formatter -> t -> unit

  (* concretization function. we call it a spawner.
     useful to do tests, and to reuse the results.
     values are generated randomly *)
  val spawn : t -> Csp.instance
end
