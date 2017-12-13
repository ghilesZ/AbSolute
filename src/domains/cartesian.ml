(* This module provides a wrapper for a cartesion representation *)
(* of the search space. It is parametrized by a domain (constraint-sense) *)
(* representation: to each variable we associate a domain *)

open Bot
open Itv_sig
open Csp
open Tools

(*******************)
(* GENERIC FUNCTOR *)
(*******************)

module Box (I:ITV) = struct

  module I = I

  (************************************************************************)
  (* TYPES *)
  (************************************************************************)

  (* maps each variable to a (non-empty) interval *)
  type t = I.t VMap.t

  let find v a =
    try (VMap.find v a, v) with
      Not_found -> (VMap.find (v^"%") a, v^"%")

  let is_integer var = var.[String.length var - 1] = '%'

  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)

  let print fmt a =
    VMap.iter (fun v i -> Format.fprintf fmt "%s:%a\n" v I.print i) a

  (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)
  (* NOTE: all binary operations assume that both boxes are defined on
     the same set of variables;
     otherwise, an Invalid_argument exception will be raised *)

  let join (a:t) (b:t) : t =
    VMap.map2z (fun _ x y -> I.join x y) a b

  let meet (a:t) (b:t) : t =
    VMap.map2z (fun _ x y -> debot(I.meet x y)) a b

  (* predicates *)
  (* ---------- *)

  let is_bottom (a:t) =
    VMap.exists (fun _ v -> I.check_bot v = Bot.Bot) a

  (* mesure *)
  (* ------ *)

  (* variable with maximal range *)
  let max_range (a:t) : var * I.t =
    VMap.fold
      (fun v i (vo,io) ->
        if (I.range i) > (I.range io) then v,i else vo,io
      ) a (VMap.min_binding a)

  (* variable with maximal range if real or with smallest range if integer *)
  let mix_range (a:t) : var * I.t =
    VMap.fold
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
    VMap.fold (fun _ x v -> (I.range x) *. v) a 1.


  (* split *)
  (* ----- *)

  let split_along (a:t) (v:var) : t list =
    let i = VMap.find v a in
    let i_list =
      if is_integer v then I.split_integer i (I.mean i)
      else I.split i (I.mean i)
    in
    List.fold_left (fun acc b ->
        match b with
        | Nb e -> (VMap.add v e a)::acc
        | Bot -> acc
      ) [] i_list

  let split (a:t) : t list =
    let (v,_) = mix_range a in
    (if !Constant.debug then
       Format.printf " ---- splits along %s ---- \n" v);
    split_along a v

  let prune (a:t) (b:t) : t list * t =
    let goods,nogood =
      VMap.fold2 (fun v i_a i_b (sures,unsure) ->
          let s,u = I.prune i_a i_b in
          let newunsure = VMap.add v u unsure
          and newsure =
            List.fold_left (fun acc e ->
                (VMap.add v e unsure)::acc
              ) sures s
          in (newsure,newunsure)
        ) a b ([],a)
    in goods,nogood

  module Hc4Normal = Hc4.Regular(I)
  module Hc4Opt = Hc4.Filters(I)

  let filter (a:t) (e1,binop,e2) : t =
    Hc4Normal.test a e1 binop e2

  let to_filterer (e1,binop,e2) : t -> t =
    Hc4Opt.test e1 binop e2

  let empty : t = VMap.empty

  let add_var abs (typ,var,dom) : t =
    let itv =
      match dom with
      | Finite (l,u) -> I.of_floats l u
      | Set (h::tl) -> List.fold_left (fun acc e -> I.join acc (I.of_float e)) (I.of_float h) tl
      | _ -> failwith "can only handle finite non-empty domains"
    in
    VMap.add (if typ = INT then (var^"%") else var) itv abs

  let is_enumerated a =
    VMap.for_all (fun v i -> (is_integer v |> not) || I.is_singleton i) a

  let spawn a =
    VMap.fold (fun k v acc -> VMap.add k (I.spawn v) acc) a VMap.empty


end

(*************)
(* INSTANCES *)
(*************)

module BoxF = Box(Itv.ItvF)
