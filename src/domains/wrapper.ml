module Make(D:Domain_signature.AbstractCP) = struct

  type t = {
      search_space  : D.t;
      constraints   : (Csp.bexpr * Csp.bexpr) list;
      (* the constraint and their complementary *)
    }

  type frontier = (Csp.bexpr * Csp.bexpr) list * D.t list

  let init (problem:Csp.prog) =
    let open Csp in
    let search_space = List.fold_left D.add_var D.empty Csp.(problem.init) in
    {
      search_space;
      (* we calculate the negation of the constraints only once,
         at initialization *)
      constraints =
        List.map
          (fun c -> c, (Csp.neg_bexpr c))
          problem.Csp.constraints;
    }

  let is_small {search_space} = D.is_small search_space

  let filter sp cstr =
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
    aux sp cstr

  let filter_bot sp constraints =
    try Bot.Nb(filter sp constraints)
    with Bot.Bot_found -> Bot.Bot

  let propagation ({search_space; constraints} as dom) : t =
    {dom with search_space =
                List.fold_left (fun sp (c,_) -> filter sp c)
                               search_space
                               constraints}

  let exploration ({search_space} as dom) =
    let splited = D.split search_space in
    List.map (fun sp -> {dom with search_space=sp}) splited

  (* for each unsatisfied constraint, we store its nogoods,
     i.e the over-approx of of the values that dont satisfy it *)
  let build_topology {search_space; constraints} : (D.t * frontier) Bot.bot =
    try
      let filtered =
        List.fold_left
          (fun sp (c,_) -> filter sp c) search_space constraints
      in
      if D.is_bottom filtered then raise Bot.Bot_found
      else
        let res =
          List.fold_left
            (fun ((sp,(cstrs,nogoods)) as acc) (c,nc) ->
              match filter_bot sp nc with
              | Bot.Bot ->
                 (* constraint satisfied, we discard it
                  and return the accumulator unchanged *)
                 acc
              | Bot.Nb b ->
                 (* constraint unsatisfied, we keep it, and we keep the nogoods *)
                 (sp,((c,nc)::cstrs,(b::nogoods)))
            ) (search_space,([],[])) constraints
        in Bot.Nb res
    with Bot.Bot_found -> Bot.Bot

  type consistency =
    | Empty
    | Full of t
    | Maybe of frontier * t

  let consistency abs =
    match build_topology abs with
    | Bot.Bot -> Empty
    | Bot.Nb (elm,([],[])) -> Full {abs with search_space = elm}
    | Bot.Nb (elm,((cstrs,nogoods) as frontier)) ->
       Maybe (frontier,{search_space=elm; constraints= cstrs})

  (* using elimination technique *)
  let prune (domain:t) (frontier:frontier) : t list * t list =
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
  let volume {search_space} = D.volume search_space

  (* test *)
  let spawn {search_space; constraints} = D.spawn search_space

  (* printer *)
  let print fmt {search_space; constraints} =
    Format.fprintf fmt "%a\n%!" D.print search_space

end

module WCBoxF = Make(Cartesian.BoxF)
