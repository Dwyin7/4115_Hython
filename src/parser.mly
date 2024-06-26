%{
open Ast
%}

// token declaration
// %token INDENT DEDENT
%token LPAREN RPAREN LBRACE RBRACE SEMI COLON COMMA EOF
%token PLUS MINUS TIMES MATMUL DIVIDE MODULO ASSIGN EQ NEQ LT LEQ GT GEQ
%token AND OR NOT DOT LBRACK RBRACK
%token <string> ID

%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <bool> BOOL_LITERAL
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL

// primitive types
%token PRIMITIVE_INT 
%token PRIMITIVE_FLOAT 
%token PRIMITIVE_BOOL
%token PRIMITIVE_CHAR
%token PRIMITIVE_STRING
%token VOID
%token FUNCTION 

// tensor types 
%token INT FLOAT BOOL CHAR STRING


%token IF ELIF ELSE FOR WHILE CONTINUE BREAK IN PASS
%token FUNC RETURN YIELD
%token TRY RAISE EXCEPT AS
%token LAMBDA
%token CUSTOM
%token FROM IMPORT

// define associativity
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE MATMUL MODULO


%start program_rule
%type <Ast.program> program_rule
%%


program_rule:
  | import_rule_list stmt_rule_list EOF { {imports = $1; globals = $2} }

import_rule_list:
  | /* empty */       { [] }
  | import_rule import_rule_list  { $1 :: $2 }

import_rule:
  | IMPORT ID SEMI                { Import($2, "") }
  | FROM ID IMPORT ID SEMI        { Import($2, $4) }

typ_rule:
// primitive types
    PRIMITIVE_INT  { P_int }
  | PRIMITIVE_FLOAT { P_float }
  | PRIMITIVE_BOOL { P_bool }
  | PRIMITIVE_CHAR { P_char }
  | PRIMITIVE_STRING { P_string }
  | VOID { Void }
  // tensor types
  | INT  { T_int }
  | FLOAT { T_float }
  | BOOL { T_bool }
  | CHAR { T_char }
  | STRING { T_string }
  | FUNCTION { Func }


expr_rule:
    INT_LITERAL { Int_literal($1) }
  | FLOAT_LITERAL { Float_literal($1) }
  | CHAR_LITERAL { Char_literal($1) }
  | STRING_LITERAL { String_literal($1) }
  | BOOL_LITERAL { Bool_literal($1) }
  | expr_rule PLUS expr_rule { Binop($1, Add, $3) }
  | expr_rule MINUS expr_rule { Binop($1, Sub, $3) }
  | expr_rule TIMES expr_rule { Binop($1, Mul, $3) }
  | expr_rule MATMUL expr_rule { Binop($1, Matmul, $3) }
  | expr_rule DIVIDE expr_rule { Binop($1, Div, $3) }
  | expr_rule EQ expr_rule { Binop($1, Equal, $3) }
  | expr_rule NEQ expr_rule { Binop($1, Neq, $3) }
  | expr_rule LT expr_rule { Binop($1, Less, $3) }
  | expr_rule LEQ expr_rule { Binop($1, Leq, $3) }
  | expr_rule GT expr_rule { Binop($1, Greater, $3) }
  | expr_rule GEQ expr_rule { Binop($1, Geq, $3) }
  | expr_rule AND expr_rule { Binop($1, And, $3) }
  | expr_rule OR expr_rule { Binop($1, Or, $3) }
  | ID { Id($1) }
  | LBRACK tensor_rule_list RBRACK { Tensor($2) }
  | ID LPAREN args_opt RPAREN { Call($1, $3) }
  | ID LBRACK expr_rule RBRACK { TensorAccess($1, $3) }
  /*| expr_rule LPAREN args_opt RPAREN { Call( $1, $3) }*/
  | LAMBDA LPAREN formal_list_rule RPAREN COLON expr_rule { Lambda($3, $6) }


args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr_rule  { [$1] }
  | expr_rule COMMA args { $1::$3 }



stmt_rule_list:
  /* nothing */  { [] }
 | stmt_rule stmt_rule_list  { $1 :: $2 }

// statment rule
stmt_rule:
    expr_rule SEMI { Expr($1) }
  | LBRACE stmt_rule_list RBRACE { Block($2) }
  | vdecl_rule { $1 }
  | fdecl_rule { $1 }
  | ID ASSIGN expr_rule SEMI { Assign($1, $3) }
  | RETURN SEMI { Return(Noexpr) }
  | RETURN expr_rule SEMI { Return($2) }
  | WHILE LPAREN expr_rule RPAREN stmt_rule      { While($3, $5) }
  | FOR LPAREN ID IN expr_rule RPAREN stmt_rule    { For($3, $5, $7) }
  | IF LPAREN expr_rule RPAREN stmt_rule        { If($3, $5) }
  | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule       { IfElse($3, $5, $7) } //if else parsing
  | IF LPAREN expr_rule RPAREN stmt_rule { If($3, $5) }

vdecl_rule:
    typ_rule ID SEMI                  { Bind($1, $2) }
  | typ_rule ID ASSIGN expr_rule SEMI { BindAndAssign(($1,$2),$4) }

// function declaration
/* def int foo(int x): */
fdecl_rule: 
  FUNC typ_rule ID LPAREN formal_list_rule RPAREN LBRACE stmt_rule_list RBRACE 
  {  
  Func({ ret_type=$2;fname= $3;params= $5;body= $8 })
  }


formal_list_rule:
  /* nothing */               { [] }
  | formal_rule               { [$1] }
  | formal_rule COMMA formal_list_rule { $1 :: $3 }

formal_rule:
  typ_rule ID      { ($1,$2) }



// e.g. [[1,2,3], [4,5,6]]
tensor_rule_list:
  /* nothing */ { [] }
  | expr_rule        { [$1] }  //1 element case:  [1] or [[1],[2]]
  | expr_rule COMMA tensor_rule_list  { $1 :: $3 }



