(* This module provides constraint rewriting utilities.
   It is able to perform simplification over multivariate polynoms
   and multiple occurences of a same expression *)
open Csp
open Tools

module CoEnv = Map.Make(struct type t = expr let compare = compare end)
module P    = Polynom.Float

let reverse_map (m1 : string CoEnv.t) : expr VMap.t =
  CoEnv.fold (fun k v env ->
      VMap.add v k env
    ) m1 VMap.empty

exception Empty

(* special variables begin with a '%' character to avoid name clash *)
let fresh_name =
  let cpt = ref 0 in
  fun () ->
  incr cpt;
  "%_"^(string_of_int (!cpt))

(* We convert a tree expression to a polynomial form.
   sub-expression does not correspond to a polynom are bound to fresh
   variables *)
let rec simplify env expr : (P.t * string CoEnv.t) =
  let check_var e env =
    try
      let var = CoEnv.find e env in
      (P.of_var var), env
    with Not_found ->
      let new_var = fresh_name() in
      let newenv = CoEnv.add e new_var env in
      (P.of_var new_var),newenv
  in
  let p,env =
  match expr with
  | Var v -> (P.of_var v),env
  | Float c -> (P.of_float c),env
  | Int c -> (P.of_int c),env
  | Binary (op,e1,e2) ->
     let p1,env' = simplify env e1 in
     let p2,env'' = simplify env' e2 in
     (match op with
      | ADD -> (P.add p1 p2),env''
      | SUB -> (P.sub p1 p2),env''
      | MUL -> (P.mul p1 p2),env''
      | DIV ->
         (* only division by a constant to make sure we do not shadow any dbz *)
         (match P.div p1 p2 with
         | Some p -> p,env''
         | None ->
           let e1 = polynom_to_expr p1 env'' and e2 = polynom_to_expr p2 env'' in
           let e = Binary (DIV,e1,e2) in
           check_var e env'')
      | POW ->
         (* only constant exponentiation *)
         (match P.pow p1 p2 with
         | Some p -> p,env''
         | None ->
         let e1 = polynom_to_expr p1 env'' and e2 = polynom_to_expr p2 env'' in
         let e = Binary (POW,e1,e2) in
         check_var e env''))
  | Unary (u,e) ->
     let p,env = simplify env e in
     (match u with
      | NEG -> (P.neg p),env
      | ABS ->
         let e = polynom_to_expr p env in
         let e = Unary(ABS,e) in
         check_var e env)
  | FunCall(name,args) ->
     let p_args,env' =
       List.fold_left (fun (pargs,env) arg ->
           let p,e = simplify env arg in
           (p::pargs,e)
         ) ([],env) args
     in
     let args' = List.rev_map (fun p -> polynom_to_expr p env') p_args in
     let e = FunCall (name, args') in
     check_var e env'
  in (P.clean p),env

(* polynom to expression conversion *)
and polynom_to_expr (p:P.t) (fake_vars: string CoEnv.t) : Csp.expr =
  let constant_handling c =
    if ceil c = c then Int (int_of_float c)
    else Float c
  in
  let fake_vars = reverse_map fake_vars in
  let of_id id =
    try VMap.find id fake_vars
    with Not_found -> Var id
  in
  let var_to_expr ((id,exp):P.var) : expr =
    (* let rec iter acc = function
     *   | 0 -> acc
     *   | n -> iter (Binary(MUL, acc, of_id id)) (n-1)
     * in *)
    match (int_of_float exp) with
    | 0 -> Int 1
    | 1 -> of_id id
    | 2 -> let var = of_id id in
           Binary(MUL,var,var)
    | n -> Binary(POW,(of_id id),Int n)
  in
  let cell_to_expr ((c,v) as m) =
    if P.is_monom_constant m then
      constant_handling c
    else if c = 1. then
      match v with
      | h::tl -> List.fold_left (fun acc e ->
          Binary(MUL,acc,(var_to_expr e))
        ) (var_to_expr h) tl
      | _ -> assert false
    else
      List.fold_left (fun acc e ->
          Binary(MUL,acc,(var_to_expr e))
        ) (constant_handling c) v
  in
  match p with
  | [] -> Int 0
  | h::tl -> List.fold_left
               (fun acc c -> Binary(ADD, acc, cell_to_expr c))
               (cell_to_expr h)
               tl

(* simplify the polynomial part of a constraint *)
let rewrite (cmp,e1,e2) : (cmpop * expr * expr) =
  (* we move e2 to left side to perform potentially more simplifications *)
  let p1,env1 = simplify CoEnv.empty e1 in
  let p2,env2 = simplify env1 e2 in
  let polynom = P.clean (P.sub p1 p2) in
  let simplified_left = polynom_to_expr polynom env2 in
  let e2 = Int 0 in
  (cmp,simplified_left,e2)
