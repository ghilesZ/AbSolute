open Domain_signature

(* Solver *)
module Solve(Abs : AbstractCP) = struct

  (* Module that handles consistency, split and elimanation *)
  include Splitter.Make(Abs)

  (* Module that handles few informations over the result of the solving *)
  module Res = Result.Make(Abs)

  (* propagation/split loop *)
  let explore (abs:Abs.t) (constrs:Csp.constrs) =
    let open Res in
    let rec aux abs cstrs res depth =
      match consistency abs cstrs with
      | Empty     -> res
      | Full abs' -> add_s res abs'
      | Maybe(abs',cstrs,comp) ->
         if Abs.is_small abs' || depth >= !Constant.max_iter -1 then add_u res abs'
         else
           List.fold_left (fun res elem ->
               aux elem cstrs (incr_step res) (depth +1)
	           ) res (split abs' cstrs)
    in
    aux abs constrs empty_res 0

  (* propagation/elimination/split loop*)
  let explorepruning (abs:Abs.t) (constrs:Csp.constrs) =
    let open Res in
    let rec aux abs cstrs res depth =
      match consistency abs cstrs with
      | Empty     -> res
      | Full abs' -> add_s res abs'
      | Maybe(abs',cstrs,comp) ->
         if Abs.is_small abs' || depth >= !Constant.max_iter -1 then add_u res abs'
         else
           let ls,lu = prune_topo abs' comp in
           let res = List.fold_left (fun r x -> add_s r x) res ls in
           List.fold_left (fun res x ->
               List.fold_left (fun res elem ->
                   aux elem cstrs (incr_step res) (depth +1)
                 ) res (split x cstrs)
	           ) res lu
    in
    aux abs constrs empty_res 0


  (* entry point of the solver *)
  let solving prob =
    let abs = init prob in
    Format.printf "abs = %a\tvolume = %f@." Abs.print abs (Abs.volume abs);
    let res = (if !Constant.pruning then explorepruning else explore) abs prob.Csp.constraints in
    Format.printf "\nsolving ends\n%!%a" Res.print res;
    res
end
