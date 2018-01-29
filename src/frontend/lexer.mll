
{
 open Lexing
 open Csp
 open Parser


(* keyword table *)
let kwd_table = Hashtbl.create 10
let _ =
  List.iter (fun (a,b) -> Hashtbl.add kwd_table a b)
   [
     "init",           TOK_INIT;
     "constraints",    TOK_CONSTR;
     "solutions",      TOK_SOL;
     "int",            TOK_INT;
     "real",           TOK_REAL;
     "oo",             TOK_INF;
     "-oo",            TOK_MINF
   ]

let parse_const = float_of_string
}



(* character classes *)
let space = [' ' '\t' '\r']+
let newline = "\n" | "\r" | "\r\n"
let digit = ['0'-'9']
let const = ( digit+ | "." digit+ | digit+ "." digit* )

rule token = parse

(* identifier or reserved keyword *)
| ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* '%'? as id
{ try Hashtbl.find kwd_table id with Not_found -> TOK_id id }


(* symbols *)
| "("    { TOK_LPAREN }
| ")"    { TOK_RPAREN }
| "{"    { TOK_LBRACE }
| "}"    { TOK_RBRACE }
| "["    { TOK_LBRACKET }
| "]"    { TOK_RBRACKET }
| ","    { TOK_COMMA }
| ";"    { TOK_SEMICOLON }
| "+"    { TOK_PLUS }
| "-"    { TOK_MINUS }
| "*"    { TOK_MULTIPLY }
| "/"    { TOK_DIVIDE }
| "^"    { TOK_POW }
| "<"    { TOK_LESS }
| ">"    { TOK_GREATER }
| "<="   { TOK_LESS_EQUAL }
| ">="   { TOK_GREATER_EQUAL }
| "=="   { TOK_EQUAL_EQUAL }
| "!="   { TOK_NOT_EQUAL }
| "="    { TOK_ASSIGN }
| "&&"   { TOK_AND }
| "||"   { TOK_OR }
| "|"    { TOK_PIPE }
| "!"    { TOK_NOT }
| ":"    { TOK_COLON }
(* literals *)
| const as c { TOK_const (float_of_string c) }

(* spaces, comments *)
| "/*" { comment lexbuf; token lexbuf }
| "//" [^ '\n' '\r']* { token lexbuf }
| newline { new_line lexbuf; token lexbuf }
| space { token lexbuf }

(* end of file *)
| eof { TOK_EOF }

(* nested comments (handled recursively)  *)
and comment = parse
| "*/" { () }
| [^ '\n' '\r'] { comment lexbuf }
| newline { new_line lexbuf; comment lexbuf }
