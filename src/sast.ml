(* make it possible to assigned function to variables (parameter types * return type) *)

(* expressions *)
type sexpr = typ * sx

and sx =
  | SNoexpr
  | SInt_literal of int
  | SFloat_literal of float
  | SBool_literal of bool
  | SChar_literal of char
  | SString_literal of string
  (* tensor *)
  | STensor of sexpr list
  | SId of id
  | SBinop of sexpr * bop * sexpr
  (* function call *)
  | SCall of sexpr * sexpr list
    (*expr * expr list instead of Id * expr list because lambda function is an expression *)
  | SLambda of bind list * sexpr

(* Parameters * body, lambda must be single-lined, the value of the single expression is the return value, the return type is inferred by the compiler*)
(*Lambda function is anomoyous but can be assigned to a func variable *)

(* statements   *)
type sstmt =
  | SBlock of sstmt list
  (* assigment *)
  | SAssign of id * sexpr
  | SBind of bind
  | SBindAndAssign of bind * sexpr
  (* function declare  *)
  | SFunc of typ * id * bind list * sstmt list
  | SIf of sexpr * sstmt
  | SExpr of sexpr
  | SWhile of sexpr * sstmt
  | SFor of id * sexpr * sstmt
  | SReturn of sexpr

type program = { imports : import list; globals : sstmt list }

(* Pretty-printing functions *)
