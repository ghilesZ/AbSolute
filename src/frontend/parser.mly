%{
open Csp
%}


/* tokens */
%token TOK_LBRACE
%token TOK_RBRACE
%token TOK_LBRACKET
%token TOK_RBRACKET
%token TOK_LPAREN
%token TOK_RPAREN
%token TOK_PIPE
%token TOK_COMMA
%token TOK_SEMICOLON
%token TOK_COLON
%token TOK_PLUS
%token TOK_MINUS
%token TOK_MULTIPLY
%token TOK_DIVIDE
%token TOK_POW
%token TOK_LESS
%token TOK_GREATER
%token TOK_LESS_EQUAL
%token TOK_GREATER_EQUAL
%token TOK_EQUAL_EQUAL
%token TOK_NOT_EQUAL
%token TOK_ASSIGN
%token TOK_AND
%token TOK_OR
%token TOK_NOT
%token TOK_INT
%token TOK_REAL
%token TOK_INIT
%token TOK_CONSTR
%token TOK_MINF
%token TOK_INF

%token <string> TOK_id
%token <float> TOK_const

%token TOK_EOF

/* priorities */
%left TOK_OR TOK_AND
%nonassoc TOK_NOT
%left TOK_PLUS TOK_MINUS
%left TOK_MULTIPLY  TOK_DIVIDE
%nonassoc unary_minus
%nonassoc TOK_POW

%type <typ> typ
%type <dom> init
%type <assign> decl
%type <bexpr> bexpr
%type <Csp.prog> file

/* entry point */
%start file

%%

file:
  domains
  constraints
  TOK_EOF
  {{init=$1;
    constraints=$2;
  }}

domains:
 | TOK_INIT TOK_LBRACE decls TOK_RBRACE {$3}

constraints:
 | TOK_CONSTR TOK_LBRACE bexprs TOK_RBRACE {$3}

varlist:
  | TOK_id varlist {$1::$2}
  | {[]}

decls:
  | decl decls {$1::$2}
  | {[]}

bexprs:
  | bexpr TOK_SEMICOLON bexprs {$1::$3}
  | {[]}

typ:
  | TOK_INT       {INT}
  | TOK_REAL      {REAL}

decl:
  | typ TOK_id TOK_ASSIGN init TOK_SEMICOLON
    { ($1, $2, $4) }

init:
  | TOK_LBRACKET TOK_MINF TOK_SEMICOLON TOK_INF TOK_RBRACKET     {Top}
  | TOK_LBRACKET TOK_MINF TOK_SEMICOLON const TOK_RBRACKET       {Minf ($4)}
  | TOK_LBRACKET const TOK_SEMICOLON TOK_INF TOK_RBRACKET        {Inf ($2)}
  | TOK_LBRACKET const TOK_SEMICOLON const TOK_RBRACKET          {Finite($2,$4)}

const:
  | TOK_const {$1}
  | TOK_MINUS TOK_const {(-.$2)}

bexpr:
  | expr cmp expr                       {Cmp ($2, $1, $3)}
  | bexpr TOK_OR bexpr                  {Or ($1,$3)}
  | bexpr TOK_AND bexpr                 {And ($1,$3)}
  | TOK_NOT bexpr                       {Not ($2)}
  | TOK_LPAREN bexpr TOK_RPAREN         { $2 }


expr:
  | TOK_LPAREN expr TOK_RPAREN          { $2 }
  | binop_expr                          { $1 }
  | TOK_MINUS expr %prec unary_minus    { Unary (NEG, $2) }
  | TOK_PIPE expr TOK_PIPE              { Unary (ABS, $2) }
  | TOK_id TOK_LPAREN args TOK_RPAREN   { FunCall ($1,$3) }
  | leaf                                { $1 }


args:
  | expr                {[$1]}
  | expr TOK_COMMA args {$1::$3}


leaf:
  | TOK_const                           { Cst $1 }
  | TOK_id                              { Var $1 }

binop_expr:
  | expr TOK_POW expr  {Binary (POW,$1,$3)}
  | binop_expr2        {$1}

binop_expr2:
  | expr TOK_DIVIDE   expr  {Binary(DIV,$1,$3)}
  | expr TOK_MULTIPLY expr  {Binary(MUL,$1,$3)}
  | binop_expr3             {$1}

binop_expr3:
  | expr TOK_PLUS  expr {Binary(ADD,$1,$3)}
  | expr TOK_MINUS expr {Binary(SUB,$1,$3)}

cmp:
  | TOK_LESS                    { LT }
  | TOK_GREATER                 { GT }
  | TOK_LESS_EQUAL              { LEQ }
  | TOK_GREATER_EQUAL           { GEQ }
  | TOK_ASSIGN                  { EQ }
  | TOK_NOT_EQUAL               { NEQ }
