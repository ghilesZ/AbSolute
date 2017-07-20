(* This module provides constraint rewriting utilities.
   It is able to perform simplification over multivariate polynoms
   and multiple occurences of a same expression *)
open Csp
module Env = Map.Make(String)
module CoEnv = Map.Make(struct type t = expr let compare = compare end)
module PI = Polynom.Int

let reverse_map (m1 : string CoEnv.t) : expr Env.t =
  CoEnv.fold (fun k v env ->
      Env.add v k env
    ) m1 Env.empty

exception Empty

(* sepcial variables begin with a '%' character to avoid name clash *)
let fresh_name =
  let cpt = ref 0 in
  fun () ->
  incr cpt;
  "%reserved_"^(string_of_int (!cpt))

(* We convert a tree expression to a polynomial form.
   sub-expression does not correspond to a polynom are bound to symbolic
   variables *)
let rec simplify env : expr -> (PI.t * string CoEnv.t) =
  let check_var e env =
    try  let var = CoEnv.find e env in (PI.of_var var),env
    with Not_found ->
      let new_var = fresh_name() in
      let newenv = CoEnv.add e new_var env in
      (PI.of_var new_var),newenv
  in
  function
  | Var v -> (PI.of_var v),env
  | Cst c -> (PI.of_float c),env
  | Binary (op,e1,e2) ->
     let p1,env' = simplify env e1 in
     let p2,env'' = simplify env' e2 in
     (match op with
      | ADD -> (PI.add p1 p2),env''
      | SUB -> (PI.sub p1 p2),env''
      | MUL -> (PI.mul p1 p2),env''
      | DIV ->
         (* only division by a constant to make sure we do not shadow any dbz *)
         (match PI.div p1 p2 with
         | Some p -> p,env''
         | None ->
           let e1 = polynom_to_expr p1 env'' and e2 = polynom_to_expr p2 env'' in
           let e = Binary (DIV,e1,e2) in
           check_var e env'')
      | POW ->
         (match PI.pow p1 p2 with
         | Some p -> p,env''
         | None ->
         (*TODO: add constant exponentiation *)
         let e1 = polynom_to_expr p1 env'' and e2 = polynom_to_expr p2 env'' in
         let e = Binary (POW,e1,e2) in
         check_var e env''))
  | Unary (u,e) ->
     let p,env = simplify env e in
     (match u with
      | NEG -> (PI.neg p),env
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
     let args' = List.rev_map (fun p -> polynom_to_expr p env) p_args in
     let e = FunCall (name, args') in
     check_var e env'

(* polynom to expression conversion *)
and polynom_to_expr (p:PI.t) (fake_vars: string CoEnv.t) : Csp.expr =
  let fake_vars = reverse_map fake_vars in
  let of_id id =
    try Env.find id fake_vars
    with Not_found -> Var id
  in
  let var_to_expr ((id,exp):PI.var) : expr =
    let rec iter acc = function
      | 0 -> acc
      | n -> iter (Binary(MUL,acc,(of_id id))) (n-1)
    in
    match exp with
    | 0 -> Cst 1.
    | n -> iter (of_id id) (n-1)
  in
  let cell_to_expr ((c,v) as m) =
    if PI.is_monom_constant m then Cst (float_of_int c)
    else if c = 1 then
      match v with
      | h::tl -> List.fold_left (fun acc e ->
          Binary(MUL,acc,(var_to_expr e))
        ) (var_to_expr h) tl
      | _ -> assert false
    else
      List.fold_left (fun acc e ->
          Binary(MUL,acc,(var_to_expr e))
        ) (Cst (float_of_int c)) v
  in
  match p with
  | [] -> Cst 0.
  | h::tl -> List.fold_left (fun acc c -> Binary(ADD,acc,(cell_to_expr c)))
                            (cell_to_expr h)
                            tl

(* simplify the polynomial part of a constraint *)
let rewrite (cmp,e1,e2) : (cmpop * expr * expr) =
  (* we move e2 to left side to perform a bit more simplifications *)
  let left = Binary(SUB,e1,e2) in
  let polynom,fake_variables = simplify CoEnv.empty left in
  let polynom = PI.clean polynom in
  let simplified_left = polynom_to_expr polynom fake_variables in
  let e2 = Cst 0. in
  (cmp,simplified_left,e2)
