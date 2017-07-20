(* This modules defines sound operators for trogonometrical functions *)

(* We use the partial approximation by series to do the computations
   to ensure soundness.
   Computation are done with the Q module of zarith
   to enjoy multi-precision rationals and to avoid rounding errors. *)

module MP = struct
  type t = Q.t

  (* few multi-precision arithmetic utilities *)
  let zero      = Q.zero
  let one       = Q.one
  let minus_one = Q.minus_one
  let two       = Q.of_int 2

  let ( +@ )  = Q.add
  let ( -@ )  = Q.sub
  let ( *@ )  = Q.mul
  let ( /@ )  = Q.div
  let ( <@ )  = Q.lt
  let ( <=@ ) = Q.leq

  let of_float = Q.of_float
  let of_int = Q.of_int

  let to_float = Q.to_float
  let to_int = Q.to_int

  let even x = (Q.to_int x) mod 2 = 0

  let print fmt b = Format.fprintf fmt "%a" Q.pp_print b
end

(********************)
(* Series utilities *)
(********************)

(* \sum f(x) with x from i to n *)
let sum i n f =
  let open MP in
  let rec aux acc cur =
    if cur = n then ((f cur) +@ acc)
    else aux ((f cur) +@ acc) (cur +@ one)
  in aux zero i

(* factorial + memoization *)
let fact =
  let tbl = Hashtbl.create 1000 in
  let rec aux acc (i:MP.t) =
    let open MP in
      if i <@ zero then raise (Invalid_argument "negative factorial")
      else if i <@ two then acc
      else (aux (i *@  acc )(i -@ one))
  in
  fun i ->
  try Hashtbl.find tbl i
  with Not_found ->
    let res = aux MP.one i in
    Hashtbl.add tbl i res;
    res

(* Exponentiation by squaring + memoization *)
let pow =
  let tbl = Hashtbl.create 1000 in
  let rec aux acc x exp =
    let open MP in
    if exp <@ zero then aux acc (one/@x) (zero -@ exp)
    else if exp = zero then acc
    else if exp = MP.one then acc *@ x
    else if even exp then aux acc (x *@ x) (exp /@ two)
    else  aux (x *@ acc) (x *@ x) ((exp -@ one)/@ two)
  in
  fun (i:MP.t) (exp:MP.t) ->
  try Hashtbl.find tbl (i,exp)
  with Not_found ->
    let res = aux MP.one i exp in
    Hashtbl.add tbl (i,exp) res;
    res

(* -1 ^ i *)
let minus_one_power i = if MP.even i then MP.one else MP.minus_one

(*********************)
(* SIN APPROXIMATION *)
(*********************)

(* sinus serie *)
let sin_partial_approx x m =
  let open MP in
  sum one m
      (fun i ->
        let sign = minus_one_power (i -@ one) in
        let n = (MP.two *@ i) -@ MP.one in
        (pow x n) /@ (fact n) *@ sign
      )

(* over-approximation of sin x *)
let sin_up x n =
  let open MP in
  let m = if x <@ zero then two *@ n else (two *@ n) +@ one in
  sin_partial_approx x m

(* under-approximation of sin x *)
let sin_down x n =
  let open MP in
  let m = if x <@ zero then two *@ n else (two *@ n) +@ one in
  sin_partial_approx x (m +@ MP.one)

(* compute both under and over-approximation once for better efficiency *)
let bound_sin x n =
  let open MP in
  let m = if x <@ zero then two *@ n else (two *@ n) +@ one in
  let up = sin_partial_approx x m in
  let down =
    let i = m +@ one in
    let sign = minus_one_power (i -@ one) in
    let n = (MP.two *@ i) -@ MP.one in
    up +@ (pow x n) /@ (fact n) *@ sign
  in down,up

(*********************)
(* COS APPROXIMATION *)
(*********************)

(* cosinus serie *)
let cos_partial_approx x m =
  let open MP in
  one +@
  sum one m
      (fun i ->
        let sign = minus_one_power i in
        let n = (MP.two *@ i) in
        (pow x n) /@ (fact n) *@ sign
      )

(* over-approximation of cos x *)
let cos_down x n =
  let open MP in
  let m = if x <@ zero then two *@ n else (two *@ n) +@ one in
  cos_partial_approx x m

(* under-approximation of cos x *)
let cos_up x n =
  let open MP in
  let m = if x <@ zero then two *@ n else (two *@ n) +@ one in
  cos_partial_approx x (m +@ MP.one)

(* compute both under and over-approximation once for better efficiency *)
let bound_cos x n =
  let open MP in
  let m = if x <@ zero then two *@ n else (two *@ n) +@ one in
  let down = cos_partial_approx x m in
  let up =
    let i = m +@ one in
    let sign = minus_one_power i in
    let n = (MP.two *@ i) -@ MP.one in
    down +@ (pow x n) /@ (fact n) *@ sign
  in down,up

(*********************************)
(* ARCTAN in ]0;1] APPROXIMATION *)
(*********************************)

(* over-approximation of artcan x, with x in ]0;1] *)
let atan_0_1_up x n =
  let open MP in
  if zero <@ x && x <=@ one then
    let m = two *@ n in
    sum zero m
        (fun i ->
          let sign = minus_one_power i in
          let n = (MP.two *@ i +@ one) in
          (pow x n) /@ n *@ sign
        )
  else raise Exit

(* under-approximation of artcan x, with x in ]0;1] *)
let atan_0_1_down x n =
  let open MP in
  if zero <@ x && x <=@ one then
    let m = two *@ n +@ one in
    sum zero m
        (fun i ->
          let sign = minus_one_power i in
          let n = (MP.two *@ i +@ one) in
          (pow x n) /@ n *@ sign
        )
  else raise Exit

(* compute both under and over-approximation once for better efficiency *)
let bound_atan_0_1 x n =
  let open MP in
  if zero <@ x && x <=@ one then
    let up =
      let m = two *@ n in
      sum zero m
          (fun i ->
            let sign = minus_one_power i in
            let n = (MP.two *@ i +@ one) in
            (pow x n) /@ n *@ sign
          )
    in (up +@
          (let i = two *@ n +@ one in
           let sign = minus_one_power i in
           let n = (MP.two *@ i +@ one) in
           (pow x n) /@ n *@ sign)),up
  else raise Exit

(********************)
(* PI APPROXIMATION *)
(********************)

let pi_down n =
  let open MP in
  (of_int 16) *@ (atan_0_1_down (one /@ (MP.of_int 5)) n)
  -@ (of_int 4) *@ (atan_0_1_up (one /@ (MP.of_int 239)) n)

let pi_up n =
  let open MP in
  (of_int 16) *@ (atan_0_1_up (one /@ (MP.of_int 5)) n)
  -@ (of_int 4) *@ (atan_0_1_down (one /@ (MP.of_int 239)) n)

let pi_itv n =
  let open MP in
  let adown,aup = bound_atan_0_1 (one /@ (of_int 5)) n
  and bdown,bup = bound_atan_0_1 (one /@ (of_int 239)) n in
  let sixteen = of_int 16 and four = of_int 4 in
  (sixteen *@ adown -@ four *@bup),(sixteen *@ aup -@ four *@bdown)

(*****************************)
(* ARCTAN IN R APPROXIMATION *)
(*****************************)

(* arctan under and over approximation.
   note : mutually recursive to handle the symetry *)
let rec atan_down x n =
  let open MP in
  if x <@ zero then minus_one *@ (atan_up (minus_one *@ x) n)
  else if zero <@ x && x <@ one then atan_0_1_down x n
  else if x = zero then zero
  else (pi_down n)/@ two -@ (atan_0_1_up (one /@ x) n)

and atan_up x n =
  let open MP in
  if x <@ zero then minus_one *@ (atan_down (minus_one *@ x) n)
  else if zero <@ x && x <@ one then atan_0_1_down x n
  else if x = zero then zero
  else (pi_up n)/@ two -@ (atan_0_1_down (one /@ x) n)

let bound_atan x n = (atan_down x n),(atan_up x n)

(**********************************)
(* ARCCOS IN [-1;1] APPROXIMATION *)
(**********************************)

(* FIXME: soundness *)
let acos_up r = MP.of_float (acos (MP.to_float r))

let acos_down r = MP.of_float (acos (MP.to_float r))

(****************************************************)
(*               INTERVAL COMPUTATION               *)
(****************************************************)

type itv = MP.t * MP.t

let print fmt (l,u) = Format.fprintf fmt "[%a; %a]" MP.print l MP.print u

(* constructors and conversions *)
let of_int (a,b) = (MP.of_int a),(MP.of_int b)
let of_float (a,b) = (MP.of_float a),(MP.of_float b)

let to_float (a,b) = (MP.to_float a), (MP.to_float b)
let to_int (a,b) = (MP.to_int a),(MP.to_int b)

(* useful constants*)
let ((pi_down,pi_up) as pi_itv) = pi_itv (MP.of_int 50)
let pihalf_down      = MP.(pi_down /@ two)
let pihalf_up        = MP.(pi_up /@ two)
let twopiup          = MP.(two *@ pi_up)
let twopidown        = MP.(two *@ pi_down)
let threehalfpi_down = MP.(pihalf_down *@ (of_int 3))
let threehalfpi_up   = MP.(pihalf_up *@ (of_int 3))

let pihalf_itv       = pihalf_down, pihalf_up
let twopi_itv        = twopidown, twopiup
let threehalfpi_itv  = threehalfpi_down, threehalfpi_up

(* trigonometry on intervals *)

(* the type of monotony of a function on a given interval *)

type monotony = Incr             (* strictly increasing*)
              | Decr             (* stricly decreasing*)
              | IncrDecr of MP.t (* IncrDecr(a) means than the function f is
                                    increasing until x, and f(x) = a.
                                    then f decreases *)
              | DecrIncr of MP.t (* DecrIncr(a) means than the function f is
                                    decreasing until x, and f(x) = a.
                                    then f increases *)

(* bring the interval to [0;4pi[
   - a < b
   - interval size should be smaller than 3pi/2, raise Exit if not *)
let normalize (a,b) =
  let open MP in
  let rec aux a b =
    if threehalfpi_down <=@ (b -@ a) then raise Exit
    else if twopiup <=@ a then aux (a -@ twopiup) (b -@ twopidown)
    else if zero <=@ a then a,b
    else aux (a +@ twopidown) (b +@ twopiup)
  in aux a b

let meet (a,b) (x,y) =
  let open MP in
  a <@ y && x <@ b

(* return the monotony of the cosinus function on the given itv. Raise exit if
   the itv is bigger than 3pi/2 anyway *)
let cosmonotony itv =
  let open MP in
  try let a,b = normalize itv in
      if meet (a,b) twopi_itv then
        IncrDecr(MP.one)
      else if meet (a,b) pi_itv || meet (a,b) threehalfpi_itv then
        DecrIncr(MP.minus_one)
      else
        (* so it should be monoton *)
        if meet (pi_down,twopiup) (a,b) then Incr
        else Decr
  with Exit -> raise Exit

(* return the monotony of the sinus function on the given itv *)
let sinmonotony itv =
  let open MP in
  try let a,b = normalize itv in
      if meet (a,b) pihalf_itv then
        IncrDecr(MP.one)
      else if meet (a,b) threehalfpi_itv then
        DecrIncr(MP.minus_one)
      else
        (* so it should be monoton *)
        if meet (pi_down,twopiup) (a,b) then Incr
        else Decr
  with Exit -> raise Exit

(* given an interval i, and function f and its monotony,
   return the image of i by f *)
let itv ((a,b) as itv) monotony f_down f_up =
  match monotony itv with
  | Incr     -> (f_down a),(f_up b)
  | Decr     -> (f_down b),(f_up a)
  | IncrDecr max ->
     let a' = f_down a and b'= f_down b in
     if MP.(a' <@ b') then (a',max) else (b',max)
  | DecrIncr min ->
     let a' = f_up a and b'= f_up b in
     if MP.(a' <@ b') then (min,b') else (min,a')
  | exception Exit -> MP.(minus_one,one)

(* cosinus of an interval *)
let cos_itv i =
  let cos_down x = cos_down x (MP.of_int 50)
  and cos_up x = cos_up x (MP.of_int 50)
  in itv i cosmonotony cos_down cos_up

(* sinus of an interval *)
let sin_itv i =
  let sin_down x = sin_down x (MP.of_int 50)
  and sin_up x = sin_up x (MP.of_int 50)
  in itv i sinmonotony sin_down sin_up

(* interval acos *)
let acos_itv (l,h) =
  let open MP in
  if  h <@  MP.minus_one || one <@ l then Bot.Bot
  else
    let is_minus_one = l <@ minus_one
    and is_plus_one =  one <@ h in
    match (is_minus_one, is_plus_one) with
    | true, true -> Bot.Nb (zero, pi_up)
    | true, false -> Bot.Nb ((acos_down h), pi_up)
    | false, true -> Bot.Nb (zero, (acos_up l))
    | false, false -> Bot.Nb ((acos_down h), (acos_up l))

(* TESTING : uncomment to test. Add in utop:
   #require "zarith";;
   #use "path/to/trigo.ml";;
 *)
(* let _ = *)
(*   let cos_test (x,y) = *)
(*     let a,b = normalize ((MP.of_int x),(MP.of_int y)) in *)
(*     Format.printf "normalization[%i;%i] -> [%a;%a]\n" *)
(*                   x *)
(*                   y *)
(*                   Format.pp_print_float *)
(*                   (MP.to_float a) *)
(*                   Format.pp_print_float *)
(*                   (MP.to_float b); *)
(*     let a,b = cos_itv ((MP.of_int x),(MP.of_int y)) in *)
(*     Format.printf "cos [%i;%i] -> [%a;%a]\n" *)
(*                   x *)
(*                   y *)
(*                   Format.pp_print_float *)
(*                   (MP.to_float a) *)
(*                   Format.pp_print_float *)
(*                   (MP.to_float b) *)
(*   in *)
(*   let open MP in *)
(*   let half = MP.of_float 0.5 in *)
(*   for i = 1 to 20 do *)
(*     let sl,su = bound_sin half (MP.of_int i) in *)
(*     assert (sl <@ su); *)
(*     Format.printf "%a <= sin(0.5) <= %a\n" *)
(*                   Format.pp_print_float *)
(*                   (MP.to_float sl) *)
(*                   Format.pp_print_float *)
(*                   (MP.to_float su); *)
(*   done; *)
(*   Format.printf "\n"; *)
(*   for i = 1 to 20 do *)
(*     let cl,cu = bound_cos half (MP.of_int i) in *)
(*     assert (cl <@ cu); *)
(*     Format.printf "%a <= cos(0.5) <= %a\n" *)
(*                   Format.pp_print_float *)
(*                   (MP.to_float cl) *)
(*                   Format.pp_print_float *)
(*                   (MP.to_float cu); *)
(*   done; *)
(*   Format.printf "\n"; *)
(*   for i = 1 to 20 do *)
(*     let atl,atu = bound_atan_0_1 half (MP.of_int i) in *)
(*     assert (atl <@ atu); *)
(*     Format.printf "%a <= atan(0.5) <= %a\n" *)
(*                   Format.pp_print_float *)
(*                   (MP.to_float atl) *)
(*                   Format.pp_print_float *)
(*                   (MP.to_float atu); *)
(*   done; *)
(*   Format.printf "\n"; *)
(*   for i = 1 to 20 do *)
(*     let atl,atu = bound_atan (MP.of_int (3*i)) (MP.of_int i) in *)
(*     assert (atl <@ atu); *)
(*     Format.printf "%a <= atan(%i) <= %a\n" *)
(*                   Format.pp_print_float *)
(*                   (MP.to_float atl) *)
(*                   (3*i) *)
(*                   Format.pp_print_float *)
(*                   (MP.to_float atu); *)
(*   done; *)
(*   for i = 1 to 20 do *)
(*     cos_test (i,(i+1)) *)
(*   done; *)
(* ;; *)
