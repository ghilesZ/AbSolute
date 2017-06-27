open Apron
(******************************************************************)
(***************** Different conversion operators *****************)
(******************************************************************)

let scalar_to_mpqf = function
  | Scalar.Mpqf x -> x
  | Scalar.Float x -> Mpqf.of_float x
  | Scalar.Mpfrf x -> Mpfrf.to_mpqf x

let scalar_to_float s =
  let res = match s with
  | Scalar.Mpqf x -> Mpqf.to_float x
  | Scalar.Float x -> x
  | Scalar.Mpfrf x -> Mpfrf.to_float ~round:Mpfr.Near x
  in res

let scalar_to_int x = scalar_to_float x |> int_of_float

let coeff_to_float = function
  | Coeff.Scalar x -> scalar_to_float x
  | Coeff.Interval i -> scalar_to_float i.Interval.inf

let coeff_to_int x = coeff_to_float x |> int_of_float

(**********************)
(*   APRON Utilities  *)
(**********************)

let empty_env = Environment.make [||] [||]

(******************************************************************)
(*********************** Various operators ************************)
(******************************************************************)

(* Compute the medium value of two scalars *)
let mid inf sup =
  let mpqf_inf = scalar_to_mpqf inf
  and mpqf_sup = scalar_to_mpqf sup in
  let div_inf = Mpqf.div mpqf_inf (Mpqf.of_int 2)
  and div_sup = Mpqf.div mpqf_sup (Mpqf.of_int 2)
  in Scalar.of_mpqf (Mpqf.add div_inf div_sup)

(* Compute the middle value of an interval *)
let mid_interval itv =
  mid itv.Interval.inf itv.Interval.sup

(* Compute the euclidian distance between two scalars *)
let diam inf sup =
  let mpqf_inf = scalar_to_mpqf inf in
  let mpqf_sup = scalar_to_mpqf sup in
  Mpqf.sub mpqf_sup mpqf_inf

(* Compute the diameter of an interval *)
let diam_interval itv =
  diam itv.Interval.inf itv.Interval.sup

(* Compute the square of the euclidian distance between two arrays of floats. *)
let sq_dist tab1 tab2 =
  let sum = ref 0. in
  Array.iter2 (fun a b -> sum := !sum +. (a -. b) ** 2.) tab1 tab2;
  !sum

(* Converts a Generator0 into an array of floats. *)
let to_float_array gen size =
  let tab = Array.make size 0. in
  let gen_lin = gen.Generator0.linexpr0 in
  for i=0 to (size-1) do
    let coeff = Linexpr0.get_coeff gen_lin i in
    tab.(i) <- coeff_to_float coeff
  done;
  tab

(* Converts a Generator1 into an array of array of floats. *)
let gen_to_array gens size =
  let gen_tab = gens.Generator1.generator0_array in
  let tab = Array.make (Array.length gen_tab) (Array.make size 0.) in
  for i=0 to ((Array.length gen_tab)-1) do
    tab.(i) <- to_float_array gen_tab.(i) size
  done;
  tab

(* compute the two furthest generators *)
let maxdisttab tabpoints =
  let best1      = ref tabpoints.(0)
  and best2      = ref tabpoints.(0) in
  let best_dist = ref (-1.) in
  Array.iteri
    (fun i p1 ->
      Array.iteri (fun j p2 ->
          let cur_dist = sq_dist p1 p2 in
          if cur_dist > !best_dist then begin
              best1 := p1;
              best2 := p2;
              best_dist := cur_dist
            end
        ) tabpoints
    )
    tabpoints;
  (!best1, !best2, !best_dist)
