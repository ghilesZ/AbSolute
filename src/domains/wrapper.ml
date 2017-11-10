module Make(D:Domain_signature.AbstractCP)= struct

  type t = {search_space : D.t; constraints : Csp.bexpr list}

  let init (problem:Csp.prog) =
    let search_space = List.fold_left D.add_var D.empty Csp.(problem.init) in
    {search_space; constraints=Csp.(problem.constraints)}

  let is_small {search_space; constraints} = D.is_small search_space

  let propagation {search_space; constraints} : t =
    let rec aux (sp:D.t) =
      let open Csp in
      function
      | And (b1,b2) -> aux (aux sp b2) b1
      | Or (b1,b2) ->
         let a1 = try Some(aux sp b1) with Bot.Bot_found -> None
         and a2 = try Some(aux sp b2) with Bot.Bot_found -> None in
         (match (a1,a2) with
          | (Some a1),(Some a2) -> D.join a1 a2
          | None, (Some x) | (Some x), None -> x
          | _ -> raise Bot.Bot_found)
      | Not b -> aux sp (neg_bexpr b)
      | Cmp (binop,e1,e2) -> D.filter sp (e1,binop,e2)
    in
    {search_space = List.fold_left aux search_space constraints; constraints}

  let exploration {search_space; constraints} =
    let splited = D.split search_space in
    List.map (fun sp -> {search_space=sp; constraints}) splited

  let prune t1 t2 = failwith "todo pruning"

  let print fmt {search_space; constraints} =
    Format.fprintf fmt "%a\n%!" D.print search_space;
    List.iter (Format.fprintf fmt "%a;\n" Csp.print_bexpr) constraints

  let spawn {search_space; constraints} = D.spawn search_space


  module Topology = struct
    (* This module defines the topology of a problem : *)
    (* - where are all the solutions *)
    (* - where are for each constraint the no-goods *)
    type t = {
        (* over-approx of the solution space *)
        sols:D.t;
        (*Foreach constraint we associate the over-approx of its complementary*)
        complementary: (Csp.bexpr list) * (D.t list)
      }

    (* printer *)
    let print fmt {sols;complementary=cstrs,compl} =
      Format.fprintf fmt "good : %a\nnogoods:\n" D.print sols;
      List.iter2 (fun e1 e2 ->
          Format.fprintf fmt "%a -> %a\n" Csp.print_bexpr e1 D.print e2
        ) cstrs compl

    (* we build a topology within an abstract element *)
    (* we only keep the constraints/complmentary element that the element
     does not satisfy yet *)
    (* return Bot if the element doesnt satisfy at all the constraints *)
    let build ({search_space;constraints} as abs): t Bot.bot =
      try
        let abs' = propagation abs in
        if D.is_bottom abs'.search_space then Bot.Bot
        else
          let complementary =
            List.fold_left
              (fun (acc1,acc2) c ->
                match filter_bot abs' (Csp.neg_bexpr c) with
                | Bot.Bot -> acc1,acc2
                | Bot.Nb comp -> (c::acc1),(comp::acc2)
              ) ([],[]) constrs
          in Bot.Nb {sols=abs';complementary}
      with Bot.Bot_found -> Bot.Bot
  end

end
