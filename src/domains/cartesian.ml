(* This module provides a wrapper for a cartesian representation *)
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

  let find v a = VMap.find_fail v a
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

  (* variable with maximal range if real or with minimal if integer *)
  let mix_range (a:t) : var * I.t =
    VMap.fold
      (fun v i (vo,io) ->
        if (I.score i) > (I.score io) then v,i else vo,io
      ) a (VMap.min_binding a)

  let is_small (a:t) : bool =
    let (v,i) = max_range a in
    (I.range i) <= !Constant.precision

  let volume (a:t) : float =
    VMap.fold (fun _ x v -> (I.range x) *. v) a 1.

  (************************)
  (* splitting strategies *)
  (************************)

  let choose a = mix_range a

  let split_along (a:t) (v,i:var * I.t) : t list =
    let i_list = I.split i in
    List.fold_left (fun acc e ->
        (VMap.add v e a)::acc
      ) [] i_list

  let split (a:t) : t list =
    let ((v,i) as var) = choose a in
    (if !Constant.debug then Format.printf " ---- splits along %s ---- \n" v);
    split_along a var

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

  module Hc4 = Hc4.Make(I)

  let filter (a:t) (e1,binop,e2) : t =
    Hc4.test a e1 binop e2

  (* filtering function that also returns a candidate varibale fr splitting *)
  let filter_maxvar (a:t) (e1,binop,e2) : t * (var*float) =
    Hc4.test_maxvar a e1 binop e2

  let empty : t = VMap.empty

  let add_var abs (typ,var,dom) : t =
    let itv =
      match typ,dom with
      | INT,Finite  (l,u) -> I.of_ints (int_of_float l) (int_of_float u)
      | REAL,Finite (l,u) -> I.of_floats l u
      | _,Set (h::tl) -> List.fold_left (fun acc e -> I.join acc (I.of_float e)) (I.of_float h) tl
      | _ -> failwith "can only handle finite non-empty domains"
    in
    VMap.add var itv abs

  (*********************************)
  (* Sanity and checking functions *)
  (*********************************)

  (* returns an randomly (uniformly?) chosen instanciation of the variables *)
  let spawn a : instance =
    VMap.fold (fun k itv acc -> VMap.add k (I.spawn itv) acc) a VMap.empty

  (* given an abstraction and instance, verifies if the abstraction is implied
     by the instance *)
  let is_abstraction a (i:instance) =
    VMap.for_all (fun k value ->
        let itv = VMap.find k a in
        I.contains_float itv value
      ) i
end

(*************)
(* INSTANCES *)
(*************)

module BoxF = Box(Trigo.ItvF)
module BoxMix = Box(Trigo.ItvMix)
