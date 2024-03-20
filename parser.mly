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



