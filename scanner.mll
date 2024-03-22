{
  open Parser
  let lineno = ref 1
}
(* need to idenfity indent and dedent *)


let alpha = ['a'-'z' 'A'-'Z']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = ['0'-'9']
let id = alpha(alpha|digit|'_')* (*TODO: exclude keywords*)

(* string is double quote  *)
let string = '"' ( (ascii | escape)* as s) '"'

(* char is single quote *)
let char = ''' ( ascii | digit ) '''

let float = (digit+) ['.'] digit+
let int = digit+
let whitespace = [' ' '\t' '\r']
let newline = '\n'
let indent = ['\t']+



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
| "void" { VOID }

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
| "return" { RETURN }
| "yield" { YIELD }

(* Errors and Exception *)
| "try" { TRY }
| "raise" { RAISE }
| "except" { EXCEPT }
| "as" { AS }

(* Anonymous functions *)
| "lambda" { LAMBDA }

(* Matrix *)
| "Custom" { CUSTOM }


(* Import*)
| "from"  { FROM }
| "import"  { IMPORT }

(* Other *)
| "true" { TRUE true }
| "false" { FALSE false }
| int as lxm { INT_LITERAL(int_of_string lxm) }
| float as lxm { FLOAT_LITERAL(float_of_string lxm) }
| char as lxm { CHAR_LITERAL( String.get lxm 1 ) }
(* | escape_char as lxm{ CHAR_LITERAL( String.get (unescape lxm) 1) }
| string { STRING_LITERAL(unescape s) } *)
| id as lxm { ID(lxm) }
| eof { EOF }
(* | '"' { raise (Exceptions.UnmatchedQuotation(!lineno)) }
| _ as illegal { raise (Exceptions.IllegalCharacter(!filename, illegal, !lineno)) } *)




and comment = parse
    newline {token lexbuf}
|   eof { EOF }
|   _       {comment lexbuf}



