open Adcp_sig
open ADCP

(* Solver *)
module Solve(Abs : AbstractCP) = struct

  include Splitter.Make(Abs)
  module Res = Result.Make(Abs)

  let explore (abs:Abs.t) (constrs:Csp.constrs) =
    let open Res in
    let rec aux abs cstrs res depth =
      match build_topology abs cstrs with
      | {sols=abs'; complementary = [],[]} -> add_s res abs'
      | {sols=abs'; complementary = cstrs,comp} ->
         if Abs.is_small abs' then add_u res abs'
         else
           if !Constant.pruning then
             let ls,lu = prune_topo abs' comp in
             let res = List.fold_left (fun r x -> add_s r x) res ls in
             List.fold_left (fun res x ->
                 List.fold_left (fun res elem ->
                     aux elem cstrs (incr_step res) (depth +1)
                   ) res (split x cstrs)
	             ) res lu
           else
             List.fold_left (fun res elem ->
                 aux elem cstrs (incr_step res) (depth +1)
	             ) res (split abs' cstrs)
      | exception Bot.Bot_found -> res
    in aux abs constrs empty_res 0

  let solving prob =
    let abs = init prob in
    Format.printf "abs = %a\tvolume = %f@." Abs.print abs (Abs.volume abs);
    let res =  explore abs prob.Csp.constraints in
    Format.printf "\nsolving ends\n%!%a" Res.print res;
    res

  let solving_various prob =
    let open Csp in
    let abs = init prob in
    (* Format.printf "abs = %a" Abs.print abs; *)
    let cons = List.filter (fun e -> not (is_cons_linear e)) prob.constraints in
    (* Format.printf "\nconstraints = [";
    List.iter (Format.printf "%a ;" (print_bexpr)) prob.constraints;
    Format.printf "]@.";
    Format.printf "non linear constraints = [";
    List.iter (Format.printf "%a ;" (print_bexpr)) cons;
    Format.printf "]@."; *)
    let lcons = List.filter (fun e -> (is_cons_linear e)) prob.constraints in
    (* Format.printf "linear constraints = [";
    List.iter (Format.printf "%a ;" (print_bexpr)) lcons;
    Format.printf "]@."; *)
    let abs = List.fold_left (fun a c -> filterl a c) abs lcons in
    Format.printf "abs = %a@." Abs.print abs;
    let res = explore abs cons in
    Format.printf "\nsolving ends\n%!%a" Res.print res;
    res
end
