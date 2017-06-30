open Bot
open Bound_sig
open Itv_sig
open Csp

(*******************)
(* GENERIC FUNCTOR *)
(*******************)

module Box (I:ITV) = struct


  (************************************************************************)
  (* TYPES *)
  (************************************************************************)

  (* interval and bound inheritance *)
  module I = I
  type i = I.t

  (* maps from variables *)
  module Env = Mapext.Make(struct type t=var let compare=compare end)

  (* maps each variable to a (non-empty) interval *)
  type t = i Env.t

  let find v a =
    try (Env.find v a, v) with
      Not_found -> (Env.find (v^"%") a, v^"%")

  let is_integer var = var.[String.length var - 1] = '%'


  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)

  let print fmt a =
    let first = ref true in
    Env.iter
      (fun v i ->
	      Format.fprintf fmt "%s%s:%a"
	                     (if !first then "" else " ")
	                     v
	                     I.print i;
	      first := false
      ) a

  let float_bounds a v =
    let ((l,h), _) = find v a in
    I.of_floats l h

  (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)
  (* NOTE: all binary operations assume that both boxes are defined on
     the same set of variables;
     otherwise, an Invalid_argument exception will be raised
   *)

  let join (a:t) (b:t) : t =
    Env.map2z (fun _ x y -> I.join x y) a b

  let meet (a:t) (b:t) : t =
    Env.map2z (fun _ x y -> debot(I.meet x y)) a b

  (* predicates *)
  (* ---------- *)

  let is_bottom (a:t) =
    Env.exists (fun _ v -> I.check_bot v = Bot.Bot) a

  let subseteq (a:t) (b:t) : bool =
    Env.for_all2z (fun _ x y -> I.subseteq x y) a b

  (* mesure *)
  (* ------ *)

  (* diameter *)
  let diameter (a:t) =
    Env.fold (fun _ x v -> max (I.range x) v) a 0.

  (* variable with maximal range *)
  let max_range (a:t) : var * i =
    Env.fold
      (fun v i (vo,io) ->
        if (I.range i) > (I.range io) then v,i else vo,io
      ) a (Env.min_binding a)

  (* variable with maximal range if real or with smallest range if integer *)
  let mix_range (a:t) : var * i =
    Env.fold
      (fun v i (vo,io) ->
        if is_integer v then
          let r = I.range i in
          if (0. <> r) && ((I.range io) > r) then v,i else vo,io
        else
          vo,io
      ) a (max_range a)

  let is_small (a:t) : bool =
    let (v,i) = max_range a in
    (I.range i) <= !Constant.precision

  let volume (a:t) : float =
    let vol_bound = Env.fold (fun _ x v -> (I.range x) *. v) a 1. in
    vol_bound


  (* split *)
  (* ----- *)

  let filter_bounds (a:t) : t =
    let b = Env.mapi (fun v i ->
      if is_integer v then
	      match I.filter_bounds i with
	      | Bot -> raise Bot_found
	      | Nb e -> e
      else i) a in
    b

  let to_bot (a:I.t bot Env.t) : t bot =
    let is_bot = Env.exists (fun v i -> is_Bot i) a in
    if is_bot then Bot
    else Nb (Env.map (fun v -> match v with
    | Nb e -> e
    | _ -> failwith "should not occur"
    ) a)

  let filter_bounds_bot (a:t bot) : t bot =
    match a with
    | Bot -> Bot
    | Nb e -> Env.mapi (fun v i ->
      if is_integer v then I.filter_bounds i
      else Nb i
    ) e
    |> to_bot

  let split_along (a:t) (v:var) : t list =
    let i = Env.find v a in
    let i_list =
      if is_integer v then I.split_integer i (I.mean i)
      else I.split i (I.mean i)
    in
    List.fold_left (fun acc b ->
        match b with
        | Nb e -> (Env.add v e a)::acc
        | Bot -> acc
      ) [] i_list

  let split (a:t) : t list =
    let (v,_) = mix_range a in
    (if !Constant.debug then
       Format.printf " ---- splits along %s ---- \n" v);
    split_along a v

  let prune (a:t) (b:t) : t list * t =
    let goods,nogood =
      Env.fold2 (fun v i_a i_b (sures,unsure) ->
          let s,u = I.prune i_a i_b in
          let newunsure = Env.add v u unsure
          and newsure =
            List.fold_left (fun acc e ->
                (Env.add v e unsure)::acc
              ) sures s
          in (newsure,newunsure)
        ) a b ([],a)
    in goods,nogood
    (* Format.printf "\n\npruning %a\n and\n %a\nobtaining:\ngoods \n" print a print b; *)
    (* List.iter (Format.printf "%a\n" print) goods; *)
    (* Format.printf "--------\nnogoods\n%a\n" print nogood; *)
    (* assert false *)

  (************************************************************************)
  (* ABSTRACT OPERATIONS *)
  (************************************************************************)

  (* trees with nodes annotated with evaluation *)
  type bexpr =
    | BFunCall of var * bexpri list
    | BUnary   of unop * bexpri
    | BBinary  of binop * bexpri * bexpri
    | BVar     of var
    | BCst     of i

  and bexpri = bexpr * i

  (* interval evaluation of an expression;
     returns the interval result but also an expression tree annotated with
     intermediate results (useful for test transfer functions

     errors (e.g. division by zero) return no result, so:
     - we raise Bot_found in case the expression only evaluates to error values
     - otherwise, we return only the non-error values
   *)
  let rec eval (a:t) (e:expr) : bexpri =
    match e with
    | FunCall(name,args) ->
       let bargs = List.map (eval a) args in
       let iargs = List.map snd bargs in
       let r = debot (I.eval_fun name iargs) in
       BFunCall(name, bargs), r
    | Var v ->
        let (r, n) =
          try find v a
          with Not_found -> failwith ("variable not found: "^v)
        in
        BVar n, r
    | Cst c ->
        let r = I.of_float c in
        BCst r, r
    | Unary (o,e1) ->
       let _,i1 as b1 = eval a e1 in
       let r = match o with
         | NEG -> I.neg i1
	       | ABS -> I.abs i1
       in
       BUnary (o,b1), r
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
       BBinary (o,b1,b2), r


  (* returns a box included in its argument, by removing points such that
     the evaluation is not in the interval;
     not all such points are removed, due to interval abstraction;
     iterating eval and refine can lead to better reults (but is more costly);
     can raise Bot_found *)
  let rec refine (a:t) (e:bexpr) (x:i) : t =
    match e with
    | BFunCall(name,args) ->
       let bexpr,itv = List.split args in
       let res = I.filter_fun name itv x in
       List.fold_left2 (fun acc e1 e2 ->
           refine acc e2 e1) a (debot res) bexpr
    | BVar v ->
       (try Env.add v (debot (I.meet x (fst (find v a)))) a
        with Not_found -> failwith ("variable not found: "^v))
    | BCst i -> ignore (debot (I.meet x i)); a
    | BUnary (o,(e1,i1)) ->
        let j = match o with
          | NEG -> I.filter_neg i1 x
          | ABS -> I.filter_abs i1 x
        in
        refine a e1 (debot j)
    | BBinary (o,(e1,i1),(e2,i2)) ->
       let j = match o with
         | ADD -> I.filter_add i1 i2 x
         | SUB -> I.filter_sub i1 i2 x
         | MUL -> I.filter_mul i1 i2 x
         | DIV -> I.filter_div i1 i2 x
	       | POW -> I.filter_pow i1 i2 x
       in
       let j1,j2 = debot j in
       refine (refine a e1 j1) e2 j2

  (* test transfer function *)
  let test (a:t) (e1:expr) (o:cmpop) (e2:expr) : t bot =
    let (b1,i1), (b2,i2) = eval a e1, eval a e2 in
    let j = match o with
    | EQ -> I.filter_eq i1 i2
    | LEQ -> I.filter_leq i1 i2
    | GEQ -> I.filter_geq i1 i2
    | NEQ -> I.filter_neq i1 i2
    (*| NEQ_INT -> I.filter_neq_int i1 i2*)
    | GT -> I.filter_gt i1 i2
    (*| GT_INT -> I.filter_gt_int i1 i2*)
    | LT -> I.filter_lt i1 i2
    (*| LT_INT -> I.filter_lt_int i1 i2*)
    in
    let aux = rebot
      (fun a ->
        let j1,j2 = debot j in
        refine (refine a b1 j1) b2 j2
      ) a
    in
    filter_bounds_bot aux

  let filter (a:t) (e1,binop,e2) : t =
    match test a e1 binop e2 with
    | Bot -> raise Bot_found
    | Nb e -> e

  let filterl (a:t) (e1,binop,e2) : t =
    filter a (e1, binop, e2)

  let empty : t = Env.empty

  let add_var abs (typ,var,dom) : t =
    let itv =
      match dom with
      | Finite (l,u) -> I.of_floats l u
      | _ -> failwith "can only handle finite domains"
    in
    Env.add (if typ = INT then (var^"%") else var) itv abs

  let is_enumerated a =
    Env.for_all (fun v i -> (is_integer v |> not) || I.is_singleton i) a
end

(*************)
(* INSTANCES *)
(*************)

module BoxF = Box(Itv.ItvF)
