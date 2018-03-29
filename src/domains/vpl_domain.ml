open Vpl
open UserInterface

(*
https://git.frama-c.com/frama-c/frama-c/blob/save/feature/eva/vpl/src/plugins/value/domains/vpl/vpl_binding.ok.ml
*)

module Coeff = Scalar.Rat
module Domain = CDomain.PedraQWrapper
include Interface(Coeff)

module Expr = struct
    module Ident = UserInterface.Lift_Ident (struct
        type t = string
        let compare = Pervasives.compare
        let to_string s = s
        end)

    type t = Csp.expr

    exception Out_of_Scope

    (* TODO: handle Binary(POW, e1, e2)? *)
    let rec to_term : t -> Term.t
        = function
        | Csp.Cst f -> Term.Cte (Coeff.of_float f)
        | Csp.Var var -> Term.Var (Ident.toVar var)
        | Csp.Unary (Csp.NEG, e) -> Term.Opp (to_term e)
        | Csp.Binary (Csp.ADD, e1, e2) -> Term.Add (to_term e1, to_term e2)
        | Csp.Binary (Csp.SUB, e1, e2) -> Term.Add (to_term e1, Term.Opp (to_term e2))
        | Csp.Binary (Csp.MUL, e1, e2) -> Term.Mul (to_term e1, to_term e2)
        | _ -> Pervasives.raise Out_of_Scope
end

module VPL = struct

	(*include Interface(NCDomain.NCVPL_Unit.Q)(Expr)*)
	include Interface(Domain)(Expr)

	let translate_cmp : Csp.cmpop -> Cstr.cmpT_extended
		= function
        | Csp.EQ -> Cstr.EQ
        | Csp.LEQ -> Cstr.LE
        | Csp.GEQ -> Cstr.GE
        | Csp.NEQ -> Cstr.NEQ
        | Csp.GT -> Cstr.GT
        | Csp.LT -> Cstr.LT

    let rec to_cond : Csp.bexpr -> UserCond.t
        = function
        | Csp.Cmp (cmp, e1, e2) -> UserCond.Atom (e1, translate_cmp cmp, e2)
        | Csp.And (e1, e2) -> UserCond.BinL(to_cond e1, CWrappers.AND, to_cond e2)
        | Csp.Or (e1, e2) -> UserCond.BinL(to_cond e1, CWrappers.OR, to_cond e2)
        | Csp.Not e -> UserCond.Not (to_cond e)

end

module VplCP : Domain_signature.AbstractCP = struct

    include VPL

    let empty : t = top

    let add_var : t -> Csp.typ * Csp.var * Csp.dom -> t
        = fun p (_,var,dom) ->
        if dom = Csp.Top then p
        else
            let cond = begin match dom with
            | Csp.Finite (bi,bs) -> Csp.And
                (Csp.Cmp (Csp.LEQ, Csp.Var var, Csp.Cst bs),
                Csp.Cmp (Csp.GEQ, Csp.Var var, Csp.Cst bi))
            | Csp.Minf bs -> Csp.Cmp (Csp.LEQ, Csp.Var var, Csp.Cst bs)
            | Csp.Inf bi -> Csp.Cmp (Csp.GEQ, Csp.Var var, Csp.Cst bi)
            | _ -> Pervasives.invalid_arg "add_var"
            end
            |> to_cond
            in
            User.assume cond p

    (* TODO: how to test this? *)
    let is_small : t -> bool
        = fun p -> false

    (* Note: the last t is the intersection between the two operands *)
    let prune : t -> t -> t list * t
        = fun p1 p2 ->
        (diff p1 p2, meet p1 p2)

    let split : t -> t list
        = fun p ->
        get_regions p

    (* TODO: can we use this variable? *)
    let split_along : t -> Csp.var -> t list
        = fun _ _ -> Pervasives.failwith "split_along: unimplemented"

    let filter : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t
        = fun state (e1,cmp,e2) ->
        User.assume (to_cond (Csp.Cmp (cmp, e1, e2))) state

    (* TODO: Should return the variable with the maximal range as well. *)
    let filter_maxvar : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t * (Csp.var*float)
        = fun _ _ ->
        Pervasives.failwith "filter_maxvar: unimplemented"

    (* TODO: use Format *)
    let print : Format.formatter -> t -> unit
        = fun _ p ->
        print_endline (to_string Expr.Ident.get_string p)

    let volume : t -> float
        = fun p ->
        match size p with
        | Some value -> Scalar.Rat.to_float value
        | None -> max_float

    (* TODO: to define *)
    let spawn : t -> Csp.instance
        = fun _ ->
        Pervasives.failwith "spawn: unimplemented"

    (* TODO: to define *)
    let is_abstraction : t -> Csp.instance -> bool
        = fun _ ->
        Pervasives.failwith "is_abstraction: unimplemented"

end

let enable_debug : unit -> unit
    = fun () ->
    Handelman.Debug.enable DebugTypes.([Title ; Normal ; Detail ; MInput ; MOutput]);
    Debug.enable();
    Debug.print_enable();
    Debug.set_colors();
    PSplx.Debug.disable()
