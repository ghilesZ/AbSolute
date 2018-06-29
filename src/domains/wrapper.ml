module Make(D:Domain_signature.AbstractCP) = struct

  type t = {
      search_space  : D.t;
      constraints   : (Csp.bexpr * Csp.bexpr) list;
      (* the constraint and their complementary *)
    }

  let init (prob:Csp.prog) =
    let search_space = List.fold_left D.add_var D.empty prob.Csp.init in
    (* we compute the negation of the constraints only once,
       at initialization *)
    let constraints = List.rev_map (fun c -> c,(Csp.neg_bexpr c)) prob.Csp.constraints in
    {search_space; constraints}

  let is_small {search_space ; _} = D.is_small search_space

  let best_var ((v1,r1) as var1) ((v2,r2) as var2) =
    if r1 > r2 then var1 else var2

  (* filters an abstract value according to a constraint.
     raises Bot.Bot_found if the value is entirely filtered *)
  let filter sp cstr =
    let rec aux (sp:D.t) =
      let open Csp in
      function
      | And (b1,b2) ->
         let sp,v1 = aux sp b2 in
         let sp,v2 =  aux sp b1 in
         sp,(best_var v1 v2)
      | Or (b1,b2) ->
         let a1 = try Some(aux sp b1) with Bot.Bot_found -> None
         and a2 = try Some(aux sp b2) with Bot.Bot_found -> None in
         (match (a1,a2) with
          | (Some (a1,v1)),(Some (a2,v2)) -> (D.join a1 a2),(best_var v1 v2)
          | None, (Some x) | (Some x), None -> x
          | _ -> raise Bot.Bot_found)
      | Not b -> aux sp (neg_bexpr b)
      | Cmp (binop,e1,e2) -> D.filter_maxvar sp (e1,binop,e2)
    in
    aux sp cstr

  (* same as precedent but returns None istead of raising an exception *)
  let filter_bot sp constraints =
    try
      let filtered,maxv = filter sp constraints in
      if D.is_bottom filtered then Bot.Bot
      else Bot.Nb filtered
    with Bot.Bot_found -> Bot.Bot

  (* for each unsatisfied constraint, we store its nogoods,
     i.e the over-approx of of the values that dont satisfy it.
     we only keep the constraints/complementary element that the element
     does not satisfy yet *)
  type frontier = ((Csp.bexpr * Csp.bexpr) list * D.t list) * Csp.var

  let build_topology {search_space; constraints} : (D.t * frontier) Bot.bot =
    try
      let filtered,(vars,_) =
        List.fold_left
          (fun (sp,var) (c,_) ->
            let sp',v = filter sp c in
            sp',(best_var var v)
          ) (search_space,("dummy",0.)) constraints
      in
      if D.is_bottom filtered then Bot.Bot
      else
        let res =
          List.fold_left
            (fun ((cstrs,nogoods) as acc) ((_,nc) as c_nc) ->
              match filter_bot filtered nc with
              | Bot.Bot ->
                 (* constraint satisfied:
                    we discard it and return the accumulator unchanged *)
                 acc
              | Bot.Nb comp ->
                 (* constraint satisfied yet:
                    we keep it, and we keep the nogoods *)
                 (c_nc::cstrs,(comp::nogoods))
            ) ([],[]) constraints
        in Bot.Nb (filtered,(res,vars))
    with Bot.Bot_found -> Bot.Bot

  type consistency =
    | Empty
    | Full of t
    | Maybe of frontier * t

  let consistency abs =
    match build_topology abs with
    | Bot.Bot -> Empty
    | Bot.Nb (elm,(([],[]),v)) -> Full {abs with search_space = elm}
    | Bot.Nb (elm,(((cstrs,nogoods),v) as frontier)) ->
       Maybe (frontier,{search_space=elm; constraints= cstrs})

  let exploration ({search_space ; _} as dom) =
    let splited = D.split search_space in
    List.rev_map (fun sp -> {dom with search_space=sp}) splited

  (* using elimination technique *)
  let prune (domain:t) ((frontier,_):frontier) : t list * t list =
    let rec aux abs (c_list,nogoods) is_sure sures unsures =
      match c_list,nogoods with
      | [],[] ->
         let new_domain = {domain with search_space = abs} in
         if is_sure then
           (new_domain::sures),unsures
         else
           sures,(new_domain::unsures)
      | (c,nc)::tl, (neg::tlnogoods) ->
	       (try
	         let s,u = D.prune abs neg in
	         let s',u' = List.fold_left (fun (sures,unsures) elm ->
	           aux elm (tl,tlnogoods) is_sure sures unsures)
	           (sures,unsures) s
	         in
	         aux u (tl,tlnogoods) false s' u'
	        with Bot.Bot_found -> aux abs (tl,tlnogoods) is_sure sures unsures)
      | _ -> failwith "constraint list and nogoods list should be of the same size"
    in aux domain.search_space frontier true [] []

  (* volume of the search_space *)
  let volume {search_space ; _} = D.volume search_space

  (* test *)
  let spawn {search_space; constraints} = D.spawn search_space

  (* printer *)
  let print fmt {search_space; constraints} =
    Format.fprintf fmt "%a\n%!" D.print search_space

end

module WCBoxF = Make(Cartesian.BoxF)
