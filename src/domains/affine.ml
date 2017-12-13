open Tools
open Csp
open Itv_sig


(* generic functor *)
(* *************** *)

module Make(I:ITV) = struct

  type env = I.t VMap.t

  (* interval constant + interval coefficients *)
  type t = I.t * env

  (* constructors *)
  (* ************ *)

  let cst (a:I.t) : t = a, VMap.empty

  let zero : t = cst (I.of_float 0.)

  let one : t = cst (I.of_float 1.)

  let minus_one : t = cst (I.of_float (-1.))

  let term (c:I.t) (v:var) : t = (I.of_float 0.), VMap.singleton v c

  let var (v:var): t = term (I.of_float 1.) v

  (* printing *)
  (* ******** *)

  let print fmt aff = ()

  (* comparison *)
  (* ********** *)

  let equal ((i1,m1):t) ((i2,m2):t) =
    (i1 = i2) && m1 = m2

  (* operators *)
  (* ********* *)

  (* evaluates form in the given environment *)
  let eval (env:env) ((i,m):t) :I.t =
    VMap.fold
      (fun v i acc ->
        I.add acc (I.mul (VMap.find v env) i)
      )
      m i

  let neg ((i,m):t) : t =
    I.neg i, VMap.map I.neg m

  let add ((i1,m1):t) ((i2,m2):t) : t =
    I.add i1 i2,
    VMap.map2o
      (fun _ i -> i) (fun _ i -> i) (fun _ i1 i2 -> I.add i1 i2)
      m1 m2

  let add_cst ((i1,m1):t) (i2:I.t) : t =
    I.add i1 i2, m1


  let sub ((i1,m1):t) ((i2,m2):t) : t =
    I.sub i1 i2,
    VMap.map2o
      (fun _ i -> i) (fun _ i -> I.neg i)
      (fun _ i1 i2 -> I.sub i1 i2) m1 m2

  let sub_cst ((i1,m1):t) (i2:I.t) : t =
    I.sub i1 i2, m1


  let mul_cst ((i1,m1):t) (i2:I.t) : t =
    I.mul i1 i2, VMap.map (fun i -> I.mul i i2) m1

  (* is i positive or negative ? *)
  let itv_cst_sign (itv:I.t) = I.sign itv <> 0

  let mul (env:env) ((i1,m1) as l1:t) ((i2,m2) as l2:t) : t =
    (* case where an argument is constant *)
    if VMap.is_empty m1 then mul_cst l2 i1 else
    if VMap.is_empty m2 then mul_cst l1 i2 else
    let i1,i2 = eval env l1, eval env l2 in
    (* prefer the interval with constant sign *)
    match itv_cst_sign i1, itv_cst_sign i2 with
    | true, false -> mul_cst l2 i1
    | false, true -> mul_cst l1 i2
    | _ ->
        (* prefer the interval with smallest range *)
        if I.range i1 <= I.range i2 then mul_cst l2 i1
        else mul_cst l1 i2


  (* maps division by an interval containing zero to Division_by_zero *)
  let itv_safe_div (i1:I.t) (i2:I.t) : I.t =
    if i1 = (I.of_float 0.) then (I.of_float 0.) else
    match I.div i1 i2 with
    | Bot.Nb i, false -> i
    | _ -> raise Division_by_zero

  let div_cst ((i1,m1):t) (i2:I.t) : t =
    itv_safe_div i1 i2, VMap.map (fun i -> itv_safe_div i i2) m1

  let div (env:env) (l1:t) (l2:t) : t =
    div_cst l1 (eval env l2)


  (* substitues variables by affine forms;
     (env v) can raise Not_found, in which case the variable is unmodified
   *)
  let subst (env:var -> t) ((i,m):t) : t =
    VMap.fold
      (fun v c acc ->
        add acc (try mul_cst (env v) c  with Not_found -> term c v)
      )
      m (i,VMap.empty)


  (* substitutes v with l in (i,m) *)
  let subst_var (v:var) (l:t) ((i,m):t) : t =
    try
      let c = VMap.find v m in
      add (mul_cst l c) (i,VMap.remove v m)
    with Not_found ->
      i,m

  (* applies f to each interval *)
  let map (f:I.t -> I.t) ((i,m):t) : t =
    f i, VMap.map f m

  (* get/set coefficients *)
  let get_cst ((i,m):t) : I.t = i

  let set_cst (i:I.t) ((_,m):t) : t = i,m

  let get_var (v:var) ((_,m):t) : I.t =
    try VMap.find v m with Not_found -> (I.of_float 0.)

  let set_var (v:var) (c:I.t) ((i,m):t) : t =
    i, VMap.add v c m

  (* projection *)
  let project ((i,m):t) (vl:var list) : t =
    let mm =
      List.fold_left
        (fun mm v -> try VMap.add v (VMap.find v m) mm with Not_found -> mm)
        VMap.empty vl
    in
    i,mm


end

(* instantiations *)
(* ************** *)

module FloatAffine = Make(Itv.ItvF)
