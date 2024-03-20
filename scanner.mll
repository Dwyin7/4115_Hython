let alpha = ['a'-'z' 'A'-'Z']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = ['0'-'9']
let id = alpha(alpha|digit|'_')* (*TODO: exclude keywords*)
let string = '"' ( (ascii | escape)* as s) '"'
let char = ''' ( ascii | digit ) '''
let float = (digit+) ['.'] digit+
let int = digit+
let whitespace = [' ' '\t' '\r']
let newline = '\n'

rule token = parse
  whitespace { token lexbuf }
| newline { incr lineno; token lexbuf}
(* comment *)
| "#" { comment lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| ';' { SEMI }
| ':' { COLON }
| ',' { COMMA }


(* Operators *)
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '@' { MATMUL }
| '/' { DIVIDE }
| '%' { MODULO }
| '=' { ASSIGN }
| "==" { EQ }
| "!=" { NEQ }
| '<' { LT }
| "<=" { LEQ }
| ">" { GT }
| ">=" { GEQ }
| "and" { AND }
| "or" { OR }
| "not" { NOT }
| '.' { DOT }
| '[' { LBRACK }
| ']' { RBRACK }


(* Types *)
| "int" { PRIMITIVE_INT }
| "float" { PRIMITIVE_FLOAT }
| "bool" { PRIMITIVE_BOOL }
| "char" { PRIMITIVE_CHAR }
| "string" { PRIMITIVE_STRING }
| "INT" { INT }
| "FLOAT" { FLOAT }
| "BOOL" { BOOL }
| "CHAR" { CHAR }
| "STRING" { STRING }

(* Branch Control *)
| "if" { IF }
| "elif" { ELIF }
| "else" { ELSE }
| "for" { FOR }
| "while" { WHILE }
| "continue" { CONTINUE }
| "break" { BREAK }
| "in" { IN }
| "pass" { PASS }

(* function *)
| "def" { FUNC }
| "return" {RETURN}
| "yield" { YIELD }


(* Errors and Exception *)
| try { TRY }
| raise { RAISE }
| except { EXCEPT }
| as { AS }

(* Anonymous functions *)
| "lambda" { LAMBDA }

(* Matrix *)
| "Custom" { CUSTOM }


(* Import*)
| "from"  { FROM }
| "import"  { IMPORT }

(* Other *)








and comment = parse
    newline {token lexbuf}
|   _       {comment lexbuf}



