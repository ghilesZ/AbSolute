open Bot
open Domain_signature

(* Boolean expressions abstractions *)
module Boolean_Simple(Abs:AbstractCP) = struct
  let rec filter (value:Abs.t) = let open Csp in function
    | And (b1,b2) -> filter (filter value b2) b1
    | Or (b1,b2) ->
      let a1 = try Some(filter value b1) with Bot_found -> None
      and a2 = try Some(filter value b2) with Bot_found -> None in
      (match (a1,a2) with
      | (Some a1),(Some a2) -> Abs.join a1 a2
      | None, (Some x) | (Some x), None -> x
      | _ -> raise Bot_found)
    | Not b -> filter value (neg_bexpr b)
    | Cmp (binop,e1,e2) -> Abs.filter value (e1,binop,e2)

end

module Boolean_Disjunction (Abs:AbstractCP) = struct

  (* given d1 and d2, two non redundant disjunction
     returns the intersection of d1 and d2 *)
  let meet_disj d1 d2 =
    List.fold_left (fun acc e ->
        List.fold_left (fun acc' e' ->
            match Abs.meet e e' with
            | x -> x::acc'
            | exception Bot_found -> acc'
          ) acc d1
      ) [] d2

  (* returns the subparts of value that do not meet with to_remove *)
  let prune_disj values to_remove =
    List.fold_left (fun acc v ->
        List.fold_left (fun acc' r ->
            let goods,_ = Abs.prune v r in
            List.rev_append goods acc'
          ) acc to_remove
      ) [] values

  (* filters a value according to a constraint *)
  let rec filter (value:Abs.t) =
    let open Csp in
    function
    | And (b1,b2) ->
       let sol2 = filter value b2 in
       List.concat (List.map (fun e -> filter e b1) sol2)
    |  Or (b1,b2) ->
        let a1 = try Some(filter value b1) with Bot_found -> None
        and a2 = try Some(filter value b2) with Bot_found -> None in
        (match (a1,a2) with
         | (Some a1),(Some a2) ->
            (match meet_disj a1 a2 with
             | [] -> a1@a2
             | meet -> (prune_disj a1 meet)@a2)
         | None, (Some x) | (Some x),None -> x
         | _ -> raise Bot_found)
    | Not b -> filter value (neg_bexpr b)
    | Cmp (binop,e1,e2) -> [Abs.filter value (e1,binop,e2)]
end

(* Consistency computation and splitting strategy handling *)
module Make (Abs : AbstractCP) = struct

  include Boolean_Simple(Abs)

  type consistency =
    | Full  of Abs.t
		| Maybe of Abs.t * Csp.bexpr list * Abs.t list
		| Empty

  let init (problem:Csp.prog) : Abs.t =
    Csp.(List.fold_left Abs.add_var Abs.empty problem.init)

  (* return true if two abstract element overlap *)
  let overlap abs1 abs2 =
    try not (Abs.is_bottom (Abs.meet abs1 abs2))
    with Bot_found -> false

  (* filter function without exception but possible bottom value instead*)
  let filter_bot abs cstr =
    let open Bot in
    try
      let filtered = filter abs cstr in
      if Abs.is_bottom filtered then Bot
      else Nb filtered
    with Bot_found -> Bot

  let split abs cstrs = Abs.split abs
  (* TODO: add other splits *)

  module Topology = struct
    (* This module defines the topology of a problem : *)
    (* - where are all the solutions *)
    (* - where are for each constraint the no-goods *)
    type t = {
        (* over-approx of the solution space *)
        sols:Abs.t;
        (* Foreach constraint we associate the over-approx of its complementary *)
        complementary: (Csp.bexpr list) * (Abs.t list)
      }

    (* printer *)
    let print fmt {sols;complementary=cstrs,compl} =
      Format.fprintf fmt "good : %a\nnogoods:\n" Abs.print sols;
      List.iter2 (fun e1 e2 ->
          Format.fprintf fmt "%a -> %a\n" Csp.print_bexpr e1 Abs.print e2
        ) cstrs compl

    (* we build a topology within an abstract element *)
    (* we only keep the cstrs/complementary that an element does not satisfy *)
    (* return Bot if the element doesnt satisfy at all the constraints *)
    let build abs constrs : t bot =
      try
        let abs' = List.fold_left filter abs constrs in
        if Abs.is_bottom abs' then Bot
        else
          let complementary =
            List.fold_left
              (fun (acc1,acc2) c ->
                match filter_bot abs' (Csp.neg_bexpr c) with
                | Bot -> acc1,acc2
                | Nb comp -> (c::acc1),(comp::acc2)
              ) ([],[]) constrs
          in Nb {sols=abs';complementary}
      with Bot_found -> Bot
  end

  (* consistency computation *)
  let consistency abs constrs : consistency =
    let open Topology in
    match build abs constrs with
    | Nb {sols;complementary=[],[]} -> Full sols
    | Nb {sols;complementary=cstrs,compl} -> Maybe(sols,cstrs,compl)
    | Bot -> Empty

  (* using elimination technique *)
  let prune_topo (abs:Abs.t) (compl:Abs.t list) =
    let rec aux abs c_list is_sure sures unsures =
      match c_list with
      | [] -> if is_sure then (abs::sures),unsures
              else sures,(abs::unsures)
      | h::tl when overlap h abs ->
         (* we separate the current element into the good part and the bad part *)
         (* according to one constraint *)
	       let s,u = Abs.prune abs h in
         let s = List.filter (fun e -> not (Abs.is_bottom e)) s in
         (* we then propagate the pruning on the good elements with a flag "true"*)
	       let s',u' =
           List.fold_left (fun (sures,unsures) elm ->
	             aux elm tl is_sure sures unsures
             ) (sures,unsures) s
	       in
         (* we finish the pruning on the nogoods with the flag set to "false"*)
	       aux u tl false s' u'
      | h::tl (* h and abs dont overlap *) -> aux abs tl is_sure sures unsures
    in aux abs compl true [] []
end
