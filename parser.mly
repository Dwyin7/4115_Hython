%{
open Ast
%}

// token declaration
%token LPAREN RPAREN LBRACE RBRACE SEMI COLON COMMA EOF
%token PLUS MINUS TIMES MATMUL DIVIDE MODULO ASSIGN EQ NEQ LT LEQ GT GEQ
%token AND OR NOT DOT LBRACK RBRACK
%token <string> ID

%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <bool> TRUE
%token <bool> FALSE
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL

// primitive types
%token PRIMITIVE_INT 
%token PRIMITIVE_FLOAT 
%token PRIMITIVE_BOOL
%token PRIMITIVE_CHAR
%token PRIMITIVE_STRING

// tensor types 
%token INT FLOAT BOOL CHAR STRING


%token IF ELIF ELSE FOR WHILE CONTINUE BREAK IN PASS
%token FUNC RETURN YIELD
%token TRY RAISE EXCEPT AS
%token LAMBDA
%token CUSTOM
%token FROM IMPORT

// define associativity


%start program
%type <Ast.tokenseq> program
%%


program:
    tokens EOF { $1 }



// LRM version (probably won't compile)
program:
  imports decls EOF { $2 :: $1 }

imports: 
  /* nothing */ { [] }
 | import_list  { List.rev $1 }

import_list:
  import_decl { $1 }
 | import_list import_decl { $2 :: $1 }

import_decl:
  IMPORT STRING_LITERAL SEMI { $2 }

decls:
  /* nothing */ { [] }
 | decls fdecl { $2 :: $1 }

fdecl: /* def int foo(int x): */
  DEF typ ID LPAREN formals_opt RPAREN COLON stmt_list
   {
   outputType = $2;
   fname = $3;
   formals = $5;
   body = List.rev $8 }

formals_opt:
  /* nothing */ { [] }
 | formal_list  { List.rev $1 }

formal_list:
   typ ID      { [($1,$2)] }
 | formal_list COMMA typ ID   { ($3,$4) :: $1 }

typ:
   PRIM_INT  { DataT(IntT) }
 | BOOL { DataT(BoolT) }
 | PRIM_FLOAT { DataT(FloatT) }
 | CHAR { DataT(CharT) }
 | VOID { DataT(VoidT) }
 | INT  { TensorT(IntT) }
 | FLOAT { TensorT(FloatT) }
 | DEF typ LPAREN func_input_opt RPAREN
 {
   FunctionT {
	input:$4;
	output:$2
   }
 }

func_input_opt:
   /* nothing */ { [] }
  | func_input_list { List.rev $1 }

func_input_list:
    typ {[$1]}
  | func_input_list COMMA typ { $3 :: $1 }

stmt_list:
  /* nothing */  { [] }
 | stmt_list stmt { $2 :: $1 }
stmt:
   expr SEMI { Expr $1 }
 | RETURN SEMI { Return Noexpr }
 | RETURN expr SEMI { Return $2 }
 | IF expr COLON stmt %prec NOELSE { If($2, $4, Block([])) }
 | IF expr COLON stmt ELSE stmt { If($2, $4, $6) }
 | IF expr COLON stmt ELIF expr COLON stmt ELSE stmt { If($2, $4, If($6, $8, $10)) }
 | FOR ID IN expr COLON stmt  { For($2, $4, $6) }
 | WHILE expr COLON stmt  { While($2, $4) }
 | TRY COLON stmt EXCEPT id COLON stmt { Try($3, $5, $7) }

expr_list:
  /* nothing */ { [] }
 | expr_list COMMA expr { $3 :: $1 }

expr:
   INT_LITERAL    { IntLit($1) }
 | FLOAT_LITERAL  { FloatLit($1) }
 | CHAR_LITERAL   { CharLit($1) }
 | STRING_LITERAL { StringLit($1) }
 | TRUE           { BoolLit(true) }
 | FALSE          { BoolLit(false) }
 | ID %prec NOACCESS  { Id($1) }
 | LBRACK expr_list RBRACK { TensorLit(List.rev $2) }
 | CUSTOM LPAREN LBRACK actuals_opt RBRACK COMMA expr RPAREN
 | expr LBRACK expr RBRACK { TensorAccess($1,$3) }
 | expr PLUS expr  { Binop($1, Add, $3) }
 | expr MINUS expr { Binop($1, Sub, $3) }
 | expr TIMES expr { Binop($1, Mult, $3) }
 | expr MATMUL expr { Binop($1, Matmul, $3) }
 | expr DIVIDE expr { Binop($1, Div, $3) }
 | expr MODULO expr { Binop($1, Mod, $3) }
 | expr EQ expr   { Binop($1, Equal, $3) }
 | expr NEQ expr  { Binop($1, Neq, $3) }
 | expr LT expr   { Binop($1, Less, $3) }
 | expr LEQ expr  { Binop($1, Leq, $3) }
 | expr GT expr   { Binop($1, Greater, $3) }
 | expr GEQ expr   { Binop($1, Geq, $3) }
 | expr AND expr   { Binop($1, And, $3) }
 | expr OR expr    { Binop($1, Or, $3) }
 | MINUS expr %prec NEG { Unop(Neg, $2) }
 | NOT expr   { Unop(Not, $2) }
 | PLUS expr $prec POS { Unop(Pos, $2) }
 | typ ID ASSIGN expr  { Assign(Immut, ($1, $2), $4) }
 | expr AS typ     { Cast($3, $1) }
 | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
 | LPAREN expr RPAREN { $2 }
 | LAMBDA formals_opt COLON stmt { formals = $2; body = $4 }

actuals_opt:
  /* nothing */ { [] }
 | actuals_list  { List.rev $1 }

actuals_list:
  expr        { [$1] }
 | actuals_list COMMA expr { $3 :: $1 }











// test only
tokens:
   /* nothing */ { [] }
 | one_token tokens { $1 :: $2 }

one_token:
  SEMI { "SEMI" }
| LPAREN { "LPAREN" }
| RPAREN { "RPAREN" }
| LBRACE { "LBRACE" }
| RBRACE { "RBRACE" }
| COMMA { "COMMA" }
| PLUS { "PLUS" }
| MINUS { "MINUS" }
| TIMES { "TIMES" }
| MATMUL { "MATMUL" }
| DIVIDE { "DIVIDE" }
| MODULO { "MODULO" }
| ASSIGN { "ASSIGN" }
| EQ { "EQ" }
| NEQ { "NEQ" }
| LT { "LT" }
| LEQ { "LEQ" }
| GT { "GT" }
| GEQ { "GEQ" }
| AND { "AND" }
| OR { "OR" }
| NOT { "NOT" }
| DOT { "DOT" }
| LBRACK { "LBRACK" }
| RBRACK { "RBRACK" }
| IF { "IF" }
| ELIF { "ELIF" }
| ELSE { "ELSE" }
| FOR { "FOR" }
| WHILE { "WHILE" }
| CONTINUE { "CONTINUE" }
| BREAK { "BREAK" }
| IN { "IN" }
| PASS { "PASS" }
| FUNC { "FUNC" }
| RETURN { "RETURN" }
| YIELD { "YIELD" }
| TRY { "TRY" }
| RAISE { "RAISE" }
| EXCEPT { "EXCEPT" }
| AS { "AS" }
| LAMBDA { "LAMBDA" }
| CUSTOM { "CUSTOM" }
| FROM { "FROM" }
| IMPORT { "IMPORT" }
| PRIMITIVE_INT { "PRIMITIVE_INT" }
| PRIMITIVE_FLOAT { "PRIMITIVE_FLOAT" }
| PRIMITIVE_BOOL { "PRIMITIVE_BOOL" }
| PRIMITIVE_CHAR { "PRIMITIVE_CHAR" }
| PRIMITIVE_STRING { "PRIMITIVE_STRING" }
| INT { "INT" }
| FLOAT { "FLOAT" }
| BOOL { "BOOL" }
| CHAR { "CHAR" }
| STRING { "STRING" }
| INT_LITERAL { "LITERAL: " ^ string_of_int $1 }
| FLOAT_LITERAL { "LITERAL: " ^ string_of_float $1 }
| CHAR_LITERAL { "LITERAL: " ^ $1 }
| STRING_LITERAL {"String lit"}
| ID { "ID: " ^ $1 }



