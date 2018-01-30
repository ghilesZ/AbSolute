open Csp
open Tools

module Make(Abs : Domain_signature.AbstractCP) = struct

  include Solver.Solve(Abs)

  (****************)
  (*   PRINTING   *)
  (****************)

  let print_instance fmt instance =
    let bindings = VMap.bindings instance in
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
      | Var v -> VMap.find v instance
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
      | FunCall(name, [e]) ->
         let e = aux e in
         let func =
           match name with
           | "sqrt" -> sqrt
           | "cos"  -> cos
           | "sin"  -> sin
           | "tan"  -> tan
           | "exp"  -> exp
           | "log"  -> log
           | x -> failwith (Format.sprintf "unrecognized function name %s" x)
          in func e
      | FunCall(name, args) -> failwith (Format.sprintf "cant evaluate function call %s" name)
    in aux expr

  (* check if an instance is valid wrt to a constraint *)
  let check_cstr print instance cstr =
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
         if not res then
           if print then err_constr instance cstr e1' op e2';
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
    let value = VMap.find var instance in
    check_type typ value && check_dom dom value

  (* checks if an instance satisfies a csp *)
  let check_instance print instance csp =
    List.for_all (belong_to instance) csp.init
    && List.for_all (check_cstr print instance) csp.constraints

  (* checks that the sure value DO satisfy the constraints *)
  let check_sure csp result =
    let total_sure = ref 0 in
    iter_sure (fun e ->
        incr total_sure;
        let i = Abs.spawn e in ignore (check_instance true i csp)
      ) result;
    !total_sure

  (* compute the ratio of unsure value that DO satisfy the constraints *)
  let check_unsure csp result =
    let total = ref 0 and unsure = ref 0 in
    iter_unsure (fun e ->
        incr total;
        let i = Abs.spawn e in
        if check_instance false i csp then incr unsure
      ) result;
    (float !unsure) /. (float !total)

  (* checks that the problem's known solutions belong to an astract element *)
  (* and that sme inconsistent instance does not belong to sure solutions *)
  let check_known_solutions csp result =
    let open Result in
    let covered_by i abs_list =
      List.exists (fun e -> Abs.is_abstraction e i) abs_list
    in
    try
      List.iter (fun (instance,good) ->
          let covered_sure = covered_by instance result.sure in
          let covered_unsure = covered_by instance result.unsure in
          if good then
            if not (covered_sure || covered_unsure)
            then
              begin
                Format.eprintf "the instance %a is not covered by any abstract element\n%!"
                               print_instance instance;
                raise Exit
              end
            else ()
          else begin
              (*the instance sould not be covered by a sure element *)
              if covered_sure then
              begin
                Format.eprintf "the instance %a shouldn't be covered by an abstract element\n%!"
                               print_instance instance;
                raise Exit
              end
            end
        ) csp.solutions;
      true
    with Exit -> false

  let result csp = solving csp

end
