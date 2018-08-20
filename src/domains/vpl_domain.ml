<<<<<<< HEAD
open Vpl
open UserInterface

(*
https://git.frama-c.com/frama-c/frama-c/blob/save/feature/eva/vpl/src/plugins/value/domains/vpl/vpl_binding.ok.ml
*)

module Debug = DebugTypes.Debug(struct let name = "AbSolute" end)

module VPL_CP_Profile = Profile.Profile(struct let name = "VPL_CP" end)

module Coeff = Scalar.Rat
(*module Domain = CDomain.PedraQWrapper*)
module Domain = NCDomain.NCVPL_Cstr.Q
include MakeInterface(Coeff)

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
        | Csp.Binary (Csp.DIV, e1, e2) -> Term.Div (to_term e1, to_term e2)
        | Csp.Binary (Csp.POW, e1, Csp.Cst f) -> begin try
                let i = int_of_float f in
                let term = to_term e1 in
                Term.Prod (List.map (fun _ -> term) (Misc.range 0 i))
            with _ -> Pervasives.raise Out_of_Scope
            end
        | _ -> Pervasives.raise Out_of_Scope
end
=======
let fail () = Pervasives.failwith "VPLDomain: uninstalled"
>>>>>>> 489fbbc5f1a2c6529832ddfd187c156907abd01e

module VplCP (* : Domain_signature.AbstractCP *)= struct

    type t = unit

    let empty = ()

    let meet _ _ = fail ()

    let join _ _ = fail ()

    let is_bottom _ = fail ()

    let add_var : t -> Csp.typ * Csp.var * Csp.dom -> t
        = fun _ _ -> fail ()

    let volume : t -> float
        = fun _ -> fail ()

    let is_small : t -> bool
        = fun _-> fail ()

    (* Note: the last t is the intersection between the two operands *)
    let prune : t -> t -> t list * t
        = fun _ _ -> fail ()

    let split : t -> t list
        = fun _ -> fail ()

    (* TODO: can we use this variable? *)
    let split_along : t -> Csp.var -> t list
        = fun _ _ -> fail ()

    let filter : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t
<<<<<<< HEAD
        = fun state (e1,cmp,e2) ->
        Debug.log DebugTypes.Title (lazy "Filter");
        let cond = to_cond (Csp.Cmp (cmp, e1, e2)) in
        Debug.log DebugTypes.MInput (lazy (UserCond.to_string cond));
        User.assume cond state
=======
        = fun _ _ -> fail ()
>>>>>>> 489fbbc5f1a2c6529832ddfd187c156907abd01e

    (* TODO: Should return the variable with the maximal range as well. *)
    let filter_maxvar : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t * (Csp.var*float)
        = fun _ _ -> fail ()

    (* TODO: use Format *)
    let print : Format.formatter -> t -> unit
        = fun _ _ -> fail ()

    (* TODO: to define *)
    let spawn : t -> Csp.instance
        = fun _ -> fail ()

    (* TODO: to define *)
    let is_abstraction : t -> Csp.instance -> bool
        = fun _ -> fail ()

end

let setup_flags : unit -> unit
    = fun () -> ()

let set_lin _ = ()

let enable_debug : unit -> unit
<<<<<<< HEAD
    = fun () ->
    Vpl.Debug.enable();
    Debug.enable DebugTypes.([Title ; MInput ; MOutput ; Normal ; Detail]);
    Handelman.Debug.enable DebugTypes.([Title ; MInput ; MOutput ; Normal ; Detail]);
    HOtypes.Debug.enable DebugTypes.([Title ; MInput ; MOutput ; Normal ; Detail]);
    Pol.Debug.enable DebugTypes.([Title ; MInput ; MOutput ; Normal ; Detail]);
    Vpl.Debug.print_enable();
    Debug.print_enable();
    Debug.set_color(DebugTypes.Cyan);
    Vpl.Debug.set_colors();
    PSplx.Debug.disable()

let start_profile () =
    VPL_CP_Profile.enable();
    VPL_CP_Profile.reset();
    VPL_CP_Profile.start "vpl";
    setup_flags()

let stop_profile () =
    VPL_CP_Profile.stop "vpl"

let report () =
    Vpl.Profile.report() |> print_endline
=======
    = fun () -> ()

let start_profile () = ()
let stop_profile () = ()
let report () = ()
>>>>>>> 489fbbc5f1a2c6529832ddfd187c156907abd01e
