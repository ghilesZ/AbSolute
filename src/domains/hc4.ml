open Csp
open Bot
open Itv_sig
open Tools

(*******************)
(* GENERIC FUNCTOR *)
(*******************)

module Make (I:ITV) = struct

  (* maps each variable to a (non-empty) interval *)
  type t = I.t VMap.t

  let find v a =
    try (VMap.find v a, v) with
      Not_found -> (VMap.find (v^"%") a, v^"%")

  (* trees with nodes annotated with evaluation *)
  type evalexpr = I.t annot_expr

  (* interval evaluation of an expression;
     returns the interval result but also an expression tree annotated with
     intermediate results (useful for test transfer functions

     errors (e.g. division by zero) return no result, so:
     - we raise Bot_found in case the expression only evaluates to error values
     - otherwise, we return only the non-error values

     this is the bottom-up part of hc4
   *)
  let rec eval (a:t) (e:expr) : evalexpr =
    match e with
    | FunCall(name,args) ->
       let bargs = List.map (eval a) args in
       let iargs = List.map snd bargs in
       let r = debot (I.eval_fun name iargs) in
       AFunCall(name, bargs),r
    | Var v ->
        let (r, n) =
          try find v a
          with Not_found -> failwith ("variable not found: "^v)
        in
        AVar (n, r),r
    | Cst c ->
        let r = I.of_float c in
        ACst (c, r),r
    | Unary (o,e1) ->
       let _,i1 as b1 = eval a e1 in
       let r = match o with
         | NEG -> I.neg i1
	       | ABS -> I.abs i1
       in
       AUnary (o,b1), r
    | Binary (o,e1,e2) ->
       let _,i1 as b1 = eval a e1
       and _,i2 as b2 = eval a e2 in
       let r = match o with
         | POW -> I.pow i1 i2
         | ADD -> I.add i1 i2
         | SUB -> I.sub i1 i2
         | DIV -> debot (fst (I.div i1 i2))
         | MUL ->
            let r = I.mul i1 i2 in
            if e1=e2 then
              (* special case: squares are positive *)
              I.abs r
            else r
       in
       ABinary (o,b1,b2), r

  (* returns a box included in its argument, by removing points such that
     the evaluation is not in the interval;
     not all such points are removed, due to interval abstraction;
     iterating eval and refine can lead to better results (but is more costly);
     can raise Bot_found

     this is the top-down part of hc4
 *)
  let rec refine (a:t) ((e,x):evalexpr) : t =
    match e with
    | AFunCall(name,args) ->
       let bexpr,itv = List.split args in
       let res = I.filter_fun name itv x in
       List.fold_left2 (fun acc e1 e2 ->
           refine acc (e2,e1)) a (debot res) bexpr
    | AVar (v,_) ->
       (try VMap.add v (debot (I.meet x (VMap.find v a))) a
        with Not_found -> failwith ("variable not found: "^v))
    | ACst (c,i) -> ignore (debot (I.meet x i)); a
    | AUnary (o,(e1,i1)) ->
        let j = match o with
          | NEG -> I.filter_neg i1 x
          | ABS -> I.filter_abs i1 x
        in
        refine a (e1,(debot j))
    | ABinary (o,(e1,i1),(e2,i2)) ->
       let j = match o with
         | ADD -> I.filter_add i1 i2 x
         | SUB -> I.filter_sub i1 i2 x
         | MUL -> I.filter_mul i1 i2 x
         | DIV -> I.filter_div i1 i2 x
	       | POW -> I.filter_pow i1 i2 x
       in
       let j1,j2 = debot j in
       refine (refine a (e1,j1)) (e2,j2)

  (* test transfer function *)
  let test (a:t) (e1:expr) (o:cmpop) (e2:expr) : t  =
    let (b1,i1), (b2,i2) = eval a e1, eval a e2 in
    let j = match o with
    | EQ  -> I.filter_eq i1 i2
    | LEQ -> I.filter_leq i1 i2
    | GEQ -> I.filter_geq i1 i2
    | NEQ -> I.filter_neq i1 i2
    | GT  -> I.filter_gt i1 i2
    | LT  -> I.filter_lt i1 i2
    in
    (fun a ->
      let j1,j2 = debot j in
      refine (refine a (b1,j1)) (b2,j2)
    ) a
end