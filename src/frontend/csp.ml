open Tools

(* variables are identified by a string *)
type var = string

(* unary arithmetic operators *)
type unop = NEG | ABS

(* binary arithmetic operators *)
type binop = ADD | SUB | MUL | DIV | POW

(* arithmetic comparison operators *)
type cmpop =
  | EQ | LEQ | GEQ | NEQ | GT | LT

(* numeric expressions *)
type expr =
  | FunCall of string * expr list
  | Unary   of unop * expr
  | Binary  of binop * expr * expr
  | Var     of var
  | Int     of int
  | Float   of float

(* boolean expressions *)
type bexpr =
  | Cmp of cmpop * expr * expr
  | And of bexpr * bexpr
  | Or  of bexpr * bexpr
  | Not of bexpr

(* variable type *)
type typ = INT | REAL

type dom = Finite of float*float  (* [a  ; b] *)
         | Minf of float          (* [-oo; b] *)
         | Inf of float           (* [a  ; +oo] *)
         | Top                    (* [-oo; +oo] *)
         | Set of float list      (* {a; b; c; ...} *)

(* assign *)
type assign = (typ * var * dom)

(* declarations *)
type decls =  assign list

(* statements *)
type constrs = bexpr list

(* the instance type *)
(* we associate a float value to each variable *)
type instance = float VMap.t

(* we can annotate a problem with information on the resolution,
   to check the soundness of the solver *)
(* A solution_info is either Some (l), where l is instance list,
   of known solution and known no goods *)
(* or None, when the problem is infeasible *)
type solution_info =
  (instance * bool) list option

(* program *)
type prog = {
    init        : decls;        (* the declarations of the variables *)
    constraints : constrs;      (* the constraints of the problem *)
    solutions   : solution_info (* extra information about the solutions of te problem *)
  }

(*****************************************)
(*        USEFUL FUNCTION ON AST         *)
(*****************************************)

(* let empty = {init = []; constraints = []; solutions = Some [];} *)

let get_vars p =
  List.map (fun (_,v,_) -> v) p.init

let add_real_var csp name inf sup =
  let assign = (REAL, name, (Finite(inf,sup))) in
  {csp with init = assign::csp.init}

let add_constr csp c =
  {csp with constraints = c::csp.constraints}

(* extract to linear constraints from domain declaration :
  x = [a,b]  -->  x >= a && x <= b *)
let domain_to_constraints (_,v,d)  =
  match d with
  | Finite (l,h) ->
     let c1 = (Var v, GEQ, Float l)
     and c2 = (Var v, LEQ, Float h)
     in c1,c2
  | _ -> failwith "cant handle non-finite domains"

(* iter on expr*)
let rec iter_expr f = function
  | Binary (op,e1,e2) as b -> f b; iter_expr f e1; iter_expr f e2
  | Unary (uop,e) as u -> f u; iter_expr f e
  | x -> f x

(* iter on constraints *)
let rec iter_constr f_expr f_constr = function
  | Cmp (c,e1,e2) as constr ->
     f_constr constr;
     iter_expr f_expr e1;
     iter_expr f_expr e2
  | And (b1,b2) as constr ->
     f_constr constr;
     iter_constr f_expr f_constr b1;
     iter_constr f_expr f_constr b2
  | Or  (b1,b2) as constr ->
     f_constr constr;
     iter_constr f_expr f_constr b1;
     iter_constr f_expr f_constr b2
  | Not b as constr ->
     f_constr constr;
     iter_constr f_expr f_constr b

(* cmp operator negation *)
let neg = function
  | EQ -> NEQ
  | LEQ ->GT
  | GEQ ->LT
  | NEQ ->EQ
  | GT -> LEQ
  | LT -> GEQ

(* constraint negation *)
let rec neg_bexpr = function
  | Cmp (op,e1,e2) -> Cmp(neg op,e1,e2)
  | And (b1,b2) -> Or (neg_bexpr b1, neg_bexpr b2)
  | Or (b1,b2) -> And (neg_bexpr b1, neg_bexpr b2)
  | Not b -> b

(* boolean formules map *)
let rec booleanmap f = function
  | Cmp (op,e1,e2) ->
     let op',e1',e2' = f (op,e1,e2) in
     Cmp(op',e1',e2')
  | And (b1,b2) -> And (booleanmap f b1, booleanmap f b2)
  | Or (b1,b2) -> Or (booleanmap f b1, booleanmap f b2)
  | Not b -> Not (booleanmap f b)

(*************************************************************)
(*                         PREDICATES                        *)
(*************************************************************)

(* checks if an expression contains a variable *)
let rec has_variable = function
  | FunCall(name,args) -> List.exists has_variable args
  | Unary (u, e)  -> has_variable e
  | Binary(b, e1, e2) -> has_variable e1 || has_variable e2
  | Var _ -> true
  | Int _ | Float _ -> false

(* checks if an expression is linear *)
let rec is_linear = function
  | Unary (NEG,e) -> is_linear e
  | Binary(MUL, e1, e2) | Binary(DIV, e1, e2)
    -> not (has_variable e1 && has_variable e2) && is_linear e1 && is_linear e2
  | Binary(POW, e1, e2)
    -> not (has_variable e1 || has_variable e2)
  | Binary(_, e1, e2) -> is_linear e1 && is_linear e2
  | Var _ | Int _ | Float _ -> true
  | _ -> false

(* checks if a constraints is linear *)
let rec is_cons_linear = function
  | Cmp (_,e1,e2) -> is_linear e1 && is_linear e2
  | And (b1,b2) -> is_cons_linear b1 && is_cons_linear b2
  | Or (b1,b2) -> is_cons_linear b1 && is_cons_linear b2
  | Not b -> is_cons_linear b

(*************************************************************)
(*                    PRINTING UTILITIES                     *)
(*************************************************************)

let print_unop fmt = function
  | NEG -> Format.fprintf fmt "-"
  | ABS -> Format.fprintf fmt "abs"

let print_binop fmt = function
  | ADD -> Format.fprintf fmt "+"
  | SUB -> Format.fprintf fmt "-"
  | MUL -> Format.fprintf fmt "*"
  | DIV -> Format.fprintf fmt "/"
  | POW -> Format.fprintf fmt "^"

let print_cmpop fmt = function
  | EQ -> Format.fprintf fmt "="
  | LEQ -> Format.fprintf fmt "<="
  | GEQ -> Format.fprintf fmt ">="
  | NEQ -> Format.fprintf fmt "<>"
  | GT ->  Format.fprintf fmt ">"
  | LT -> Format.fprintf fmt "<"

let print_typ fmt = function
  | INT ->  Format.fprintf fmt "int"
  | REAL ->  Format.fprintf fmt "real"

let print_var fmt name = Format.fprintf fmt "%s" name

let print_dom fmt = function
  | Finite (a,b) ->  Format.fprintf fmt "[%.2f; %.2f]" a b
  | Minf i -> Format.fprintf fmt "[-oo; %.2f]" i
  | Inf i -> Format.fprintf fmt "[%.2f; 00]" i
  | Top -> Format.fprintf fmt "[-oo; 00]"
  | Set v ->
     let print_vals fmt =
       Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
         Format.pp_print_float fmt
     in Format.fprintf fmt "[%a]" print_vals v

let print_assign fmt (a,b,c) =
  Format.fprintf fmt "%a %a=%a" print_typ a print_var b print_dom c

let rec print_expr fmt = function
  | FunCall(name,args) ->
     let print_args fmt =
       Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         print_expr fmt
     in
     Format.fprintf fmt "%s(%a)" name print_args args
  | Unary (NEG, e) ->
    Format.fprintf fmt "(- %a)" print_expr e
  | Unary (u, e) ->
    Format.fprintf fmt "%a %a" print_unop u print_expr e
  | Binary (b, e1 , e2) ->
    Format.fprintf fmt "%a %a %a" print_expr e1 print_binop b print_expr e2
  | Var name -> Format.fprintf fmt "%s" name
  | Int i -> Format.fprintf fmt "%i" i
  | Float f -> Format.fprintf fmt "%a" Format.pp_print_float f

let rec print_bexpr fmt = function
  | Cmp (c,e1,e2) ->
    Format.fprintf fmt "%a %a %a" print_expr e1 print_cmpop c print_expr e2
  | And (b1,b2) ->
    Format.fprintf fmt "%a && %a" print_bexpr b1 print_bexpr b2
  | Or  (b1,b2) ->
    Format.fprintf fmt "%a || %a" print_bexpr b1 print_bexpr b2
  | Not b -> Format.fprintf fmt "not %a" print_bexpr b

let print fmt prog =
  let rec aux f = function
  | [] -> ()
  | a::tl -> Format.fprintf fmt "%a;\n" f a; aux f tl
  in
  aux print_assign prog.init;
  Format.fprintf fmt "\n";
  aux print_bexpr prog.constraints

let print_instance fmt i =
  Format.fprintf fmt "{";
  VMap.iter (fun k f -> Format.fprintf fmt "%s : %a " k Format.pp_print_float f) i;
  Format.fprintf fmt "}"

(****************** EXPRESSION ANNOTATIONS ********************)
(*         A unique type for the anotated expr tree           *)
(* useful to unify the treatment of tree expression traversal *)
(**************************************************************)

type 'a annot_expr = 'a ex * 'a
and 'a ex =
  | AFunCall of var   * 'a annot_expr list
  | AUnary   of unop  * 'a annot_expr
  | ABinary  of binop * 'a annot_expr * 'a annot_expr
  | AVar     of var   * 'a
  | AInt     of int   * 'a
  | AFloat   of float * 'a
