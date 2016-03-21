# 2 "src/frontend/lexer.mll"
 
 open Lexing
 open Syntax
 open Parser


(* keyword table *)
let kwd_table = Hashtbl.create 10
let _ = 
  List.iter (fun (a,b) -> Hashtbl.add kwd_table a b)
    [
      "init",           TOK_INIT;
      "constraints",    TOK_CONSTR;
      "sqrt",           TOK_SQRT;
      "cos",            TOK_COS;
      "sin",            TOK_SIN;
      "int",            TOK_INT;
      "real",           TOK_REAL
   ]

(* (exact) parsing of decimal constants constants *)
(*let parse_const c =
  let rec div10 x n =
    if n <= 0 then x else div10 (x /. (float_of_int 10)) (n-1)
  in
  try
    let p = String.index c '.' in
    let p' = String.length c - p - 1 in
    let x = (String.sub c 0 p)^(String.sub c (p+1) p') in
    div10 (float_of_string x) p'
  with Not_found ->
    float_of_string c
*)

let parse_const = float_of_string

# 39 "src/frontend/lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\226\255\002\000\007\000\228\255\078\000\090\000\013\000\
    \020\000\002\000\003\000\031\000\035\000\242\255\106\000\244\255\
    \245\255\246\255\247\255\248\255\249\255\250\255\251\255\252\255\
    \253\255\254\255\112\000\255\255\008\000\230\255\239\255\238\255\
    \237\255\236\255\234\255\233\255\187\000\197\000\161\000\002\000\
    \253\255\254\255\103\000\255\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\028\000\027\000\255\255\255\255\024\000\255\255\
    \255\255\023\000\020\000\015\000\014\000\255\255\012\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\026\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\024\000\024\000\255\255\002\000\
    \255\255\255\255\001\000\255\255";
  Lexing.lex_default = 
   "\255\255\000\000\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\028\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\255\255\041\000\255\255\
    \000\000\000\000\255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\002\000\004\000\002\000\040\000\003\000\000\000\002\000\
    \002\000\004\000\255\255\000\000\002\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\009\000\002\000\000\000\000\000\000\000\008\000\002\000\
    \025\000\024\000\015\000\017\000\019\000\016\000\005\000\014\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\034\000\018\000\012\000\010\000\011\000\033\000\
    \032\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\021\000\031\000\020\000\013\000\026\000\
    \030\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\023\000\007\000\022\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \036\000\035\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\029\000\027\000\043\000\000\000\
    \000\000\028\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\000\000\040\000\000\000\000\000\039\000\000\000\
    \000\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\042\000\000\000\000\000\000\000\026\000\
    \000\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\000\000\
    \001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\002\000\039\000\000\000\255\255\002\000\
    \003\000\003\000\028\000\255\255\003\000\028\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\002\000\255\255\255\255\255\255\000\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\008\000\000\000\000\000\000\000\000\000\009\000\
    \010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\011\000\000\000\000\000\000\000\
    \012\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \006\000\007\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\014\000\026\000\042\000\255\255\
    \255\255\014\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\255\255\038\000\255\255\255\255\038\000\255\255\
    \255\255\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\038\000\255\255\255\255\255\255\026\000\
    \255\255\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \028\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\038\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 50 "src/frontend/lexer.mll"
                                                               id
# 200 "src/frontend/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 51 "src/frontend/lexer.mll"
( try Hashtbl.find kwd_table id with Not_found -> TOK_id id )
# 204 "src/frontend/lexer.ml"

  | 1 ->
# 55 "src/frontend/lexer.mll"
         ( TOK_LPAREN )
# 209 "src/frontend/lexer.ml"

  | 2 ->
# 56 "src/frontend/lexer.mll"
         ( TOK_RPAREN )
# 214 "src/frontend/lexer.ml"

  | 3 ->
# 57 "src/frontend/lexer.mll"
         ( TOK_LBRACE )
# 219 "src/frontend/lexer.ml"

  | 4 ->
# 58 "src/frontend/lexer.mll"
         ( TOK_RBRACE )
# 224 "src/frontend/lexer.ml"

  | 5 ->
# 59 "src/frontend/lexer.mll"
         ( TOK_LBRACKET )
# 229 "src/frontend/lexer.ml"

  | 6 ->
# 60 "src/frontend/lexer.mll"
         ( TOK_RBRACKET )
# 234 "src/frontend/lexer.ml"

  | 7 ->
# 61 "src/frontend/lexer.mll"
         ( TOK_COMMA )
# 239 "src/frontend/lexer.ml"

  | 8 ->
# 62 "src/frontend/lexer.mll"
         ( TOK_SEMICOLON )
# 244 "src/frontend/lexer.ml"

  | 9 ->
# 63 "src/frontend/lexer.mll"
         ( TOK_PLUS )
# 249 "src/frontend/lexer.ml"

  | 10 ->
# 64 "src/frontend/lexer.mll"
         ( TOK_MINUS )
# 254 "src/frontend/lexer.ml"

  | 11 ->
# 65 "src/frontend/lexer.mll"
         ( TOK_MULTIPLY )
# 259 "src/frontend/lexer.ml"

  | 12 ->
# 66 "src/frontend/lexer.mll"
         ( TOK_DIVIDE )
# 264 "src/frontend/lexer.ml"

  | 13 ->
# 67 "src/frontend/lexer.mll"
         ( TOK_POW )
# 269 "src/frontend/lexer.ml"

  | 14 ->
# 68 "src/frontend/lexer.mll"
         ( TOK_LESS )
# 274 "src/frontend/lexer.ml"

  | 15 ->
# 69 "src/frontend/lexer.mll"
         ( TOK_GREATER )
# 279 "src/frontend/lexer.ml"

  | 16 ->
# 70 "src/frontend/lexer.mll"
         ( TOK_LESS_EQUAL )
# 284 "src/frontend/lexer.ml"

  | 17 ->
# 71 "src/frontend/lexer.mll"
         ( TOK_GREATER_EQUAL )
# 289 "src/frontend/lexer.ml"

  | 18 ->
# 72 "src/frontend/lexer.mll"
         ( TOK_EQUAL_EQUAL )
# 294 "src/frontend/lexer.ml"

  | 19 ->
# 73 "src/frontend/lexer.mll"
         ( TOK_NOT_EQUAL )
# 299 "src/frontend/lexer.ml"

  | 20 ->
# 74 "src/frontend/lexer.mll"
         ( TOK_ASSIGN )
# 304 "src/frontend/lexer.ml"

  | 21 ->
# 75 "src/frontend/lexer.mll"
         ( TOK_AND )
# 309 "src/frontend/lexer.ml"

  | 22 ->
# 76 "src/frontend/lexer.mll"
         ( TOK_OR )
# 314 "src/frontend/lexer.ml"

  | 23 ->
# 77 "src/frontend/lexer.mll"
         ( TOK_NOT )
# 319 "src/frontend/lexer.ml"

  | 24 ->
let
# 80 "src/frontend/lexer.mll"
           c
# 325 "src/frontend/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 80 "src/frontend/lexer.mll"
             ( TOK_const (float_of_string c) )
# 329 "src/frontend/lexer.ml"

  | 25 ->
# 83 "src/frontend/lexer.mll"
       ( comment lexbuf; token lexbuf )
# 334 "src/frontend/lexer.ml"

  | 26 ->
# 84 "src/frontend/lexer.mll"
                      ( token lexbuf )
# 339 "src/frontend/lexer.ml"

  | 27 ->
# 85 "src/frontend/lexer.mll"
          ( new_line lexbuf; token lexbuf )
# 344 "src/frontend/lexer.ml"

  | 28 ->
# 86 "src/frontend/lexer.mll"
        ( token lexbuf )
# 349 "src/frontend/lexer.ml"

  | 29 ->
# 89 "src/frontend/lexer.mll"
      ( TOK_EOF )
# 354 "src/frontend/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment lexbuf =
    __ocaml_lex_comment_rec lexbuf 38
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 93 "src/frontend/lexer.mll"
       ( () )
# 366 "src/frontend/lexer.ml"

  | 1 ->
# 94 "src/frontend/lexer.mll"
                ( comment lexbuf )
# 371 "src/frontend/lexer.ml"

  | 2 ->
# 95 "src/frontend/lexer.mll"
          ( new_line lexbuf; comment lexbuf )
# 376 "src/frontend/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

;;

