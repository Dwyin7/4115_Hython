%{
open Ast
%}

// token declaration
%token LPAREN RPAREN LBRACE RBRACE SEMI COLON COMMA EOF
%token PLUS MINUS TIMES MATMUL DIVIDE MODULO ASSIGN EQ NEQ LT LEQ GT GEQ
%token AND OR NOT DOT LBRACK RBRACK

// primitive types
%token <int> PRIMITIVE_INT 
%token <float> PRIMITIVE_FLOAT 
%token <bool> PRIMITIVE_BOOL
%token <char> PRIMITIVE_CHAR
%token <string> PRIMITIVE_STRING

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
    tokens EOF {$1}


