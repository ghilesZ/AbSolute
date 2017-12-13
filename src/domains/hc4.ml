open Csp
open Bot
open Itv_sig
open Tools

(*******************)
(* GENERIC FUNCTOR *)
(*******************)

module Regular (I:ITV) = struct

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


module Opt (I:ITV) = struct

  (* maps each variable to a (non-empty) interval *)
  type t = I.t VMap.t

  (* trees with nodes annotated with evaluation *)
  type evalexpr = (t -> I.t) annot_expr

  (* builds an evaluation function from an arithmetic expression *)
  let rec evaluator (e:expr) : evalexpr =
    match e with
    | FunCall(name,args) -> assert false
    | Var v ->
       let r = fun a ->
         (VMap.find v a) in
       AVar(v,r),r
    | Cst c ->
       let ic = I.of_float c in
       let r = fun _ -> ic in
       ACst(c,r),r
    | Unary (o,e) ->
       let _,f as e = evaluator e in
       let r =
         let op_fun =
           match o with
           | NEG -> I.neg
	         | ABS -> I.abs
         in
         fun a -> op_fun (f a)
       in AUnary(o,e),r
    | Binary (o,te1,te2) ->
       let _,f1 as e1 = evaluator te1 in
       let _,f2 as e2 = evaluator te2 in
       let r =
         let op_fun =
           match o with
           | POW -> I.pow
           | ADD -> I.add
           | SUB -> I.sub
           | DIV -> fun i1 i2 -> debot (fst (I.div i1 i2))
           | MUL ->
              fun i1 i2 ->
              let r = I.mul i1 i2 in
              if te1=te2 then
                (* special case: squares are positive *)
                I.abs r
              else r
         in
         fun a ->
         op_fun (f1 a) (f2 a)
       in
       ABinary (o,e1,e2),r

  (* this is the top-down part of hc4 *)
  let rec refiner ((e,f):evalexpr) : t -> I.t -> t =
    match e with
    | AFunCall(name,args) -> assert false
    | AVar (v,_) ->
       fun a i ->
       (try VMap.add v (debot (I.meet i (VMap.find v a))) a
        with Not_found -> failwith ("variable not found: "^v))
    | ACst (c,_) ->
       let ic = I.of_float c in
       fun a i ->
       ignore (debot (I.meet i ic));
       a
    | AUnary (o,((_,fi) as te)) ->
       let filter =
         match o with
         | NEG -> I.filter_neg
         | ABS -> I.filter_abs
       in
       let r = refiner te in
       fun a i ->
       r a (debot (filter (fi a) i))
    | ABinary (o,((_,fi1) as te1),((_,fi2) as te2)) ->
       let filter =
         match o with
         | ADD -> I.filter_add
         | SUB -> I.filter_sub
         | MUL -> I.filter_mul
         | DIV -> I.filter_div
	       | POW -> I.filter_pow
       in
       let r1 = refiner te1 and r2 = refiner te2 in
       fun a i ->
       let j = filter (fi1 a) (fi2 a) i in
       let j1,j2 = debot j in
       r2 (r1 a j1) j2

  (* let rec build_eval_filter e : (t -> I.t) * (t -> I.t -> t) = *)
  (*   match e with *)
  (*   | FunCall(name,args) -> assert false *)
  (*   | Var v -> *)
  (*      let eval = fun a -> *)
  (*        VMap.find v a *)
  (*      and filter = fun a i -> *)
  (*        VMap.add v (debot (I.meet i (VMap.find v a))) a *)
  (*      in *)
  (*      eval,filter *)
  (*   | Cst c -> *)
  (*      let itv_c = I.of_float c in *)
  (*      let eval = fun a -> *)
  (*        itv_c *)
  (*      and filter = fun a i -> *)
  (*        ignore (debot (I.meet i itv_c)); *)
  (*        a *)
  (*      in *)
  (*      eval,filter *)
  (*   | Unary(o,e) -> *)
  (*      let eval_e,filter_e = build_eval_filter e in *)
  (*      let eval_fun,filter_fun = *)
  (*          match o with *)
  (*          | NEG -> I.neg, I.filter_neg *)
	(*          | ABS -> I.abs, I.filter_abs *)
  (*      in *)
  (*      let eval = fun a -> *)
  (*        eval_fun (eval_e a) *)
  (*      and filter = fun a i -> *)
  (*        filter_fun (e) *)

  (*   | Binary (o,e1,e2) -> *)
  (*      let eval,filter = *)
  (*        match o with *)
  (*        | ADD -> I.add, I.filter_add *)
  (*        | SUB -> I.sub, I.filter_sub *)
  (*        | MUL -> I.mul, I.filter_mul *)
  (*        | DIV -> *)
  (*           (fun i1 i2 -> debot (fst (I.div i1 i2))), I.filter_div *)
	(*        | POW -> I.pow,I.filter_pow *)
  (*      in *)
  (*      let e1,f1 = build_eval_filter e1 *)
  (*      and e2,f2 = build_eval_filter e2 *)
  (*      in *)
  (*      let eval a = eval (e1 a) (e2 a) in *)
  (*      let filter a i = *)
  (*        let j = filter (f1 a) (f2 a) i in *)
  (*        let j1,j2 = debot j in *)
  (*        r2 (r1 a j1) j2) *)
  (*   | _ -> assert false *)

  (* test transfer function *)
  let test (e1:expr) (o:cmpop) (e2:expr) : t -> t =
    let ((_,f1) as e1) = evaluator e1
    and ((_,f2) as e2) = evaluator e2 in
    let r1 = refiner e1 and r2 = refiner e2 in
    let filter =  match o with
      | EQ  -> I.filter_eq
      | LEQ -> I.filter_leq
      | GEQ -> I.filter_geq
      | NEQ -> I.filter_neq
      | GT  -> I.filter_gt
      | LT  -> I.filter_lt
    in
    (* Format.printf "eval and refine computed\n%!"; *)
    fun a ->
    let itv_l = f1 a and itv_r = f2 a in
    let j = filter itv_l itv_r in
    let j1,j2 = debot j in
    r2 (r1 a j1) j2

end


 module Filters (I:ITV) = struct

  (* maps each variable to a (non-empty) interval *)
  type t = I.t VMap.t

  type evalexpr = I.t

  (* Builds an evaluation function from an arithmetic expression *)
  (* stores intermediate results into a stack for the filtering function *)
  let rec evaluator (e:expr) : t -> I.t Stack.t -> evalexpr =
    match e with
    | FunCall(name,args) -> assert false
    | Var v ->
       fun a stack ->
       let itv = VMap.find v a in
       itv
    | Cst c ->
       let ic = I.of_float c in
       fun _ stack-> ic
    | Unary (o,e) ->
       let eval = evaluator e in
       let op_fun =
         match o with
         | NEG -> I.neg
	       | ABS -> I.abs
       in
       fun a stack->
       let i = eval a stack in
       Stack.push i stack;
       op_fun i

    | Binary (o,e1,e2) ->
       let eval1 = evaluator e1 in
       let eval2 = evaluator e2 in
       let op_fun =
         match o with
         | POW -> I.pow
         | ADD -> I.add
         | SUB -> I.sub
         | DIV -> fun i1 i2 -> debot (fst (I.div i1 i2))
         | MUL ->
            fun i1 i2 ->
            let r = I.mul i1 i2 in
            if e1=e2 then
              (* special case: squares are positive *)
              I.abs r
            else r
       in
       fun a stack ->
       let i1 = eval1 a stack in
       let i2 = eval2 a stack in
       Stack.push i1 stack;
       Stack.push i2 stack;
       op_fun i1 i2

  let rec refiner (e:expr) : (t -> I.t -> I.t Stack.t -> t) =
    match e with
    | FunCall(name,args) -> assert false
    | Var v ->
       fun a i stack ->
       (VMap.add v (debot (I.meet i (VMap.find v a))) a)
    | Cst c ->
       let cst = I.of_float c in
       fun a i stack ->
       ignore (debot (I.meet i cst));
       a
    | Unary (o,e) ->
       let filter = match o with
         | NEG -> I.filter_neg
         | ABS -> I.filter_abs
       in
       let r = refiner e in
       fun a x stack ->
       let i = Stack.pop stack in
       let j = debot (filter i x) in
       r a j stack
    | Binary (o,e1,e2) ->
       let filter = match o with
         | ADD -> I.filter_add
         | SUB -> I.filter_sub
         | MUL -> I.filter_mul
         | DIV -> I.filter_div
	       | POW -> I.filter_pow
       in
       let r1 = refiner e1
       and r2 = refiner e2 in
       fun a x stack ->
       let i1 = Stack.pop stack in
       let i2 = Stack.pop stack in
       let j1,j2 = debot (filter i1 i2 x) in
       r1 (r2 a j2 stack) j1 stack

  (* test transfer function *)
  let test (e1:expr) (o:cmpop) (e2:expr) : t -> t =
    let eval1 = evaluator e1
    and eval2 = evaluator e2 in
    let refine1 = refiner e1
    and refine2 = refiner e2 in
    let filter =  match o with
      | EQ  -> I.filter_eq
      | LEQ -> I.filter_leq
      | GEQ -> I.filter_geq
      | NEQ -> I.filter_neq
      | GT  -> I.filter_gt
      | LT  -> I.filter_lt
    in
    let s1 = Stack.create() in
    let s2 = Stack.create() in
    fun a ->
    let i1 = eval1 a s1 in
    let i2 = eval2 a s2 in
    Format.printf "stack 1 : \n%!";
    Stack.iter (Format.printf "%a\n" I.print) s1;
    Format.printf "stack 2 : \n%!";
    Stack.iter (Format.printf "%a\n" I.print) s2;
    let j1,j2 = debot (filter i1 i2) in
    let a' = refine2 a j2 s2 in
    refine1 a' j1 s1
end
