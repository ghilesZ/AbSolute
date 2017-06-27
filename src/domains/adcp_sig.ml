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

  val is_enumerated : t -> bool

  (*** OPERATIONS ***)
  val join: t -> t -> t

  (* pruning *)
  val prune : t -> t -> t list * t

  (* splits an abstract element *)
  val split : t -> t list

  val filter : t -> (expr * cmpop * expr) -> t

  (* printing *)
  val print : Format.formatter -> t -> unit

  (* volume *)
  val volume : t -> float

 end
