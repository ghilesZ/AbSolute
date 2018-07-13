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
       (* Format.printf "%a\n%!" I.print r; *)
       AFunCall(name, bargs),r
    | Var v ->
       let (r, n) =
         try find v a
         with Not_found -> failwith ("variable not found: "^v)
       in
       (* Format.printf "%a\n%!" I.print r; *)
       AVar (n, r),r
    | Float c ->
       let r = I.of_float c in
       (* Format.printf "%a\n%!" I.print r; *)
       AFloat (c, r),r
    | Int c ->
       let r = I.of_int c in
       (* Format.printf "%a\n%!" I.print r; *)
       AInt (c, r),r
    | Unary (o,e1) ->
       let _,i1 as b1 = eval a e1 in
       let r = match o with
         | NEG -> I.neg i1
	       | ABS -> I.abs i1
       in
       (* Format.printf "%a\n%!" I.print r; *)
       AUnary (o,b1), r
    | Binary (o,e1,e2) ->
       let _,i1 as b1 = eval a e1
       and _,i2 as b2 = eval a e2 in
       let r = match o with
         | POW -> I.pow i1 i2
         | ADD -> I.add i1 i2
         | SUB -> I.sub i1 i2
         | DIV -> debot (I.div i1 i2)
         | MUL ->
            let r = I.mul i1 i2 in
            if e1=e2 then
              (* special case: squares are positive *)
              I.abs r
            else r
       in
       (* Format.printf "%a\n%!" I.print r; *)
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
    | AFloat (c,i) -> ignore (debot (I.meet x i)); a
    | AInt (c,i) -> ignore (debot (I.meet x i)); a
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
    let j1,j2 = match o with
      | LT  -> debot (I.filter_lt i1 i2)
      | LEQ -> debot (I.filter_leq i1 i2)
      (* a > b <=> b < a*)
      | GEQ -> let j2,j1 = debot (I.filter_leq i2 i1) in (j1,j2)
      | GT  -> let j2,j1 = debot (I.filter_lt i2 i1) in (j1,j2)
      | NEQ -> debot (I.filter_neq i1 i2)
      | EQ  -> debot (I.filter_eq i1 i2)
    in
    refine (refine a (b1,j1)) (b2,j2)

  (* test transfer function *)
  let test_maxvar (a:t) (e1:expr) (o:cmpop) (e2:expr) : (t* (var * float))  =
    let biggest_var = ref None in
    let update v i =
      match !biggest_var with
      | None -> biggest_var := Some(v,I.range i)
      | Some (v',old_range) ->
         let new_range = I.range i in
         if new_range > old_range then
           biggest_var := Some(v,new_range)
    in
    (* same as refine but also returns the biggest constrained variable *)
    let refine_maxvar (a:t) (evalexpr:evalexpr) : t =
      let rec refine (a:t) ((e,x):evalexpr) : t  =
        match e with
        | AFunCall(name,args) ->
           let bexpr,itv = List.split args in
           let res = I.filter_fun name itv x in
           List.fold_left2 (fun acc e1 e2 ->
               refine acc (e2,e1)) a (debot res) bexpr
        | AVar (v,_) ->
           (try
              let new_itv = debot (I.meet x (VMap.find v a)) in
              update v new_itv;
              VMap.add v new_itv a
            with Not_found -> failwith ("variable not found: "^v))
        | AFloat (c,i) -> ignore (debot (I.meet x i)); a
        | AInt (c,i) -> ignore (debot (I.meet x i)); a
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
      in
      refine a evalexpr
    in
    let (b1,i1), (b2,i2) = eval a e1, eval a e2 in
    let j1,j2 = match o with
      | LT  -> debot (I.filter_lt i1 i2)
      | LEQ -> debot (I.filter_leq i1 i2)
      (* a > b <=> b < a*)
      | GEQ -> let j2,j1 = debot (I.filter_leq i2 i1) in (j1,j2)
      | GT  -> let j2,j1 = debot (I.filter_lt i2 i1) in (j1,j2)
      | NEQ -> debot (I.filter_neq i1 i2)
      | EQ  -> debot (I.filter_eq i1 i2)
    in
    let a = refine_maxvar (refine_maxvar a (b1,j1)) (b2,j2) in
    match !biggest_var with
    | None -> failwith "no var in constraint"
    | Some ((v,range) as var) -> a,var

end
