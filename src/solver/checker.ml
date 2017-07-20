open Csp

module M = Map.Make(String)

type instance = {vars : float M.t}

let print_instance fmt instance =
  let bindings = M.bindings instance in
  let print_bind fmt (var,value) =
    Format.fprintf fmt "%s:%a"
                   var
                   Format.pp_print_float value
  in
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ; ")
                       (fun fmt -> Format.fprintf fmt "%a" print_bind)
                       fmt bindings

let err_constr instance cstr e1 cmp e2 =
  Format.eprintf "the instance %a do not satisfy the constraint %a\n"
                 print_instance instance
                 print_bexpr cstr;
  Format.eprintf "it evaluates to %a %a %a\n"
                 Format.pp_print_float e1
                 print_cmpop cmp
                 Format.pp_print_float e2

(* evaluate an expression according to an instance *)
let eval instance expr =
  let rec aux = function
    | Var v -> M.find v instance
    | Cst c -> c
    | Binary(op,e1,e2) ->
       let e1' = aux e1 and e2' = aux e2 in
       (match op with
        | ADD -> e1' +. e2'
        | SUB -> e1' -. e2'
        | MUL -> e1' *. e2'
        | DIV -> e1' /. e2'
        | POW -> e1' ** e2')
    | Unary(u,e) ->
       let e' = aux e in
       (match u with
        | NEG -> (-. e')
        | ABS -> abs_float e')
    | FunCall(name, args) -> failwith "cant evaluate function call for now"
  in aux expr

(* check if an instance is valid wrt to a constraint *)
let check_cstr instance cstr =
  let rec aux = function
    | Cmp(op,e1,e2) ->
       let e1' = eval instance e1 and e2' = eval instance e2 in
       let res =
         (match op with
          | EQ  -> e1' == e2'
          | NEQ -> e1' <> e2'
          | GT  -> e1' >  e2'
          | GEQ -> e1' >= e2'
          | LT  -> e1' <  e2'
          | LEQ -> e1' <= e2'
         )
       in
       if not res then err_constr instance cstr e1' op e2';
       res
    | Or(c1,c2)  -> aux c1 || aux c2
    | And(c1,c2) -> aux c1 && aux c2
    | Not c      -> not (aux c)
  in aux cstr

(*checks if the value of variable of an instance belong to the cooresponding domain *)
let belong_to instance (typ,var,dom) =
  let check_type typ value =
    match typ with
    | INT  -> ceil value = value
    | REAL -> true
  in
  let check_dom dom value =
    match dom with
    | Finite (l,u) -> l < value && value < u
    | _ -> failwith "cant handle infinite domains for now"
  in
  let value = M.find var instance in
  check_type typ value  && check_dom dom value

(* checks if an instance satisfies a csp *)
let check instance csp =
  List.for_all (belong_to instance) csp.init
  && List.for_all (check_cstr instance) csp.constraints

let of_list entries = fun instance ->
  List.fold_left (fun acc (k,v) -> M.add k v acc) instance entries

let dummy_check =
  let instance = of_list [("x",1.); ("y",1.)] M.empty in
  check instance
