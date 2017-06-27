open Apron
open Mpqf
open Format
open Apron_utils

(**
 * Module for the Box Abstract Domains for Constraint Programming.
 *)
module BoxCP =
  struct

    include Apron_domain.MAKE (struct
      type t = Box.t
      let get_manager =  Box.manager_alloc ()
    end)

    let is_small box : bool =
      let (_, _, max) = largest box in
      let dim = Mpqf.to_float max in
      (dim <= !Constant.precision)

  let split abs =
    let env = Abstract1.env abs in
    let (var, itv, size) = largest abs in
    let mid = mid_interval itv in
    let value = mid in
    (* var <= mid*)
    let expr =  Linexpr1.make env in
    Linexpr1.set_list expr [(Coeff.s_of_int (-1), var)] (Some (Coeff.Scalar (mid)));
    (* var >= value*)
    let expr' =  Linexpr1.make env in
    Linexpr1.set_list expr' [(Coeff.s_of_int 1, var)] (Some (Coeff.Scalar (Scalar.neg value)));
    split abs (expr,expr')

  let volume abs =
    let box = Abstract1.to_box man abs in
    let tab = box.Abstract1.interval_array in
    let vol = Array.fold_left (fun v itv -> Mpqf.mul v (diam_interval itv)) (Mpqf.of_int 1) tab in
    Mpqf.to_float vol
  end


(**
 * Module for the Octagon Abstract Domains for Constraint Programming.
 *)
module OctBoxCP =
  struct

    include Apron_domain.MAKE (struct
      type t = Oct.t
      let get_manager =  Oct.manager_alloc ()
    end)

    let is_small oct =
      let (_, _,max) = largest oct in
      let dim = Mpqf.to_float max in
      (dim <= !Constant.precision)

    let split octad =
      let env = Abstract1.env octad in
      let (var, itv, size) = largest octad in
      let mid = mid_interval itv in
      let value = mid in
      (* var <= mid*)
      let expr =  Linexpr1.make env in
      Linexpr1.set_list expr [(Coeff.s_of_int (-1), var)] (Some (Coeff.Scalar (mid)));
      (* var >= value*)
      let expr' =  Linexpr1.make env in
      Linexpr1.set_list expr' [(Coeff.s_of_int 1, var)] (Some (Coeff.Scalar (Scalar.neg value)));
      split octad (expr, expr')

    let volume box = 0.
  end

(**
 * Module for the Polyhedron Abstract Domains for Constraint Programming.
 *)
module PolyCP = struct
  include Apron_domain.MAKE (struct
    type t = Polka.strict Polka.t
    let get_manager = Polka.manager_alloc_strict()
  end)

  let is_small poly = is_small man poly

  let split poly = split poly (get_expr (Polka.manager_alloc_strict()) poly)

    let volume box = 0.
end
