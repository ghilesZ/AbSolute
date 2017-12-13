open Bot
open Itv_sig
open Csp
open Tools

module Box (I:ITV) = struct

  (************************************************************************)
  (* TYPES *)
  (************************************************************************)

  (* interval and bound inheritance *)
  module I = I
  type i = I.t

  (* maps each variable to a (non-empty) interval *)
  type t = i VMap.t

  module A = Affine.Make(I)

  let lin_expr (env:t) (expr:Csp.expr) =
    match expr with
    | Csp.FunCall (_,_) ->
    | Csp.Unary (_,_) ->
    | Csp.Binary (_,_,_) -> (??)
    | Csp.Var _ -> (??)
    | Csp.Cst _ -> (??)
end
