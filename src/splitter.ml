open Adcp_sig

(* Boolean expressions abstractions *)
module Boolean (Abs:AbstractCP) = struct
  
  let rec filter (value:Abs.t) = let open Syntax in function
    | And (b1,b2) -> filter (filter value b2) b1
    | Or (b1,b2) ->
      let a1 = try Some(filter value b1) with Bot.Bot_found -> None
      and a2 = try Some(filter value b2) with Bot.Bot_found -> None in
      (match (a1,a2) with
      | (Some a1),(Some a2) -> Abs.join a1 a2
      | None, (Some x) | (Some x), None -> x
      | _ -> raise Bot.Bot_found)
    | Not b -> filter value (Syntax.neg_bexpr b)
    | Cmp (binop,e1,e2) -> Abs.filter value (e1,binop,e2)

  
  let sat_cons (a:Abs.t) (constr:Syntax.bexpr) : bool =
    try Abs.is_bottom (filter a (Syntax.Not constr))
    with Bot.Bot_found -> Abs.is_enumerated a
end

(* Consistency computation and splitting strategy handling *)
module Make (Abs : AbstractCP) = struct

  include Boolean(Abs)

  type consistency = Full of Abs.t 
		     | Maybe of Abs.t * Syntax.bexpr list
		     | Empty

  let consistency abs constrs : consistency =
    try
      let abs' = List.fold_left filter abs constrs in
      let unsat = List.filter (fun c -> not (sat_cons abs' c)) constrs in
      match unsat with
      | [] -> Full abs'
      | _ -> if Abs.is_bottom abs' then Empty else Maybe(abs', unsat)
    with Bot.Bot_found -> Empty

  let split abs expr = Abs.split abs expr
    (* TODO: add other splits *)
end
