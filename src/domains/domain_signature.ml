(**********************************************************************************)
(*            Module for Abstract Domains for Constraint Programming.             *)
(*   These are abstract domains with consistency, split and precision operators.  *)
(**********************************************************************************)

open Csp

module type AbstractCP = sig

  (*** TYPES ***)
  (* abstract elements *)
  type t

  (*** INSTANCIATION ***)

  (* returns an empty element *)
  val empty : t

  (* adds an unconstrained variable to the environnement *)
  val add_var : t -> typ * var * dom -> t

  (*** PREDICATES ***)

  (* tests if an abstract element is too small to be cut *)
  val is_small : t -> bool

  val is_bottom : t -> bool

  (*** OPERATIONS ***)
  val join: t -> t -> t

  val meet: t -> t -> t

  (* pruning *)
  val prune : t -> t -> t list * t

  (* splits an abstract element *)
  val split : t -> t list

  (* filters an abstract element according to a constraint *)
  val filter : t -> (expr * cmpop * expr) -> t

  (* builds a filtering function from a constraint *)
  val to_filterer : (expr * cmpop * expr) -> (t -> t)

  (* printing *)
  val print : Format.formatter -> t -> unit

  (* volume *)
  val volume : t -> float

  (* concretization function. we call it a spawner.
     useful to do tests, and to reuse the results.
     values are generated randomly *)
  val spawn : t -> instance
end
