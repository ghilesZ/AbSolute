module Make(D:Domain_signature.AbstractCP) = struct

  (* a filtering functions takes a search space as input and return the
    filtered search_space *)
  type filter = D.t -> D.t

  (* filter function without exception but possible bottom value instead *)
  let filter_bot (f:filter) abs =
    let open Bot in
    try
      let filtered = f abs in
      if D.is_bottom filtered then Bot
      else Nb filtered
    with Bot_found -> Bot

  (* builds ONLY ONCE a filtering function f from a constraint.
     f will be used to filter the search_space *)
  let rec to_filterer : Csp.bexpr -> filter =
    let open Csp in
    function
    | And (b1,b2) ->
       let f1 = to_filterer b1 and f2 = to_filterer b2 in
       fun sp -> f2 (f1 sp)
    | Or (b1,b2) ->
       let f1 = to_filterer b1 and f2 = to_filterer b2 in
       fun sp ->
       let open Bot in
       (match (filter_bot f1 sp),(filter_bot f2 sp) with
        | (Nb a1),(Nb a2) -> D.join a1 a2
        | Bot, (Nb x) | (Nb x), Bot -> x
        | _ -> raise Bot.Bot_found)
    | Not b -> to_filterer (neg_bexpr b)
    | Cmp (binop,e1,e2) -> D.to_filterer (e1,binop,e2)

  (* The abstract value we manipulate is a search_space
     and a list of filtering function that will be called in turn
     over the space *)
  type t = {
      search_space : D.t;
      filters      : (filter * filter) list;
    }

  (* initialization of the problem *)
  let init (problem:Csp.prog) =
    let open Csp in
    (* for each constraint, we build two filtering function :
       - one that filters according to the constraint
       - one that filters according to the complementary constraint
       ex : a + b < c
       and  a + b >= c
     *)
    let filters =
      List.map (fun c ->
          (to_filterer c),(to_filterer (Csp.neg_bexpr c)))
               (problem.constraints)
    in
    let search_space = List.fold_left D.add_var D.empty (problem.init) in
    {search_space; filters}

  (* termination condition *)
  let is_small {search_space; filters} = D.is_small search_space

  (* splitting *)
  let exploration ({search_space; filters} as a) =
    let splited = D.split search_space in
    List.rev_map (fun sp -> {a with search_space=sp}) splited

  let build_topology {search_space;filters} =
    try
      let filtered = List.fold_left (fun acc (f,_) -> f acc) search_space filters in
      if D.is_bottom filtered then raise Bot.Bot_found
      else
        let complementary =
          List.fold_left
            (fun (acc1,acc2,acc3) (f,g) ->
              let acc3 = f acc3 in
              match filter_bot g acc3 with
              | Bot.Bot ->
                 (* constraint satisfied, we discard it *)
                 (acc1,acc2,acc3)
              | Bot.Nb b ->
                 (* constraint unsatisfied, we keep it, and we keep the nogoods *)
                 ((f,g)::acc1),(b::acc2),acc3
            ) ([],[],search_space) filters
        in Bot.Nb complementary
    with Bot.Bot_found -> Bot.Bot

  type consistency =
    | Empty
    | Full of t
    | Maybe of t

  let consistency abs =
    match build_topology abs with
    | Bot.Bot -> Empty
    | Bot.Nb ([],_,elm) -> Full abs
    | Bot.Nb (filters,no_goods,elm) -> Maybe {search_space=elm;filters}

  let prune abs = assert false

  (* volume of the search_space *)
  let volume {search_space} = D.volume search_space

  (* test *)
  let spawn {search_space; filters} = D.spawn search_space

  (* printer *)
  let print fmt {search_space; filters} =
    Format.fprintf fmt "%a\n%!" D.print search_space

end

module WCBoxF = Make(Cartesian.BoxF)
