

type bop = Add | Mul | Sub | Div | Equal | Neq | Less | And | Or | Matmul | Greater | Geq | Leq

type typ = P_int | P_bool | P_float | P_char | P_string | T_int | T_bool | T_float | T_char | T_string | Void
          | Func (* make it possible to assigned function to variables (parameter types * return type) *)

type id = string

(* from lib_x import func_y *)
type import = Import of id * id


type bind = typ * id

(* expressions *)
type expr =
  | Noexpr
  | Int_literal of int
  | Float_literal of float
  | Bool_literal of bool
  | Char_literal of char
  | String_literal of string
  (* tensor *)
  | Tensor of expr list 
  | Id of id 
  | Binop of expr * bop * expr
  (* function call *)
  | Call of expr * expr list (*expr * expr list instead of Id * expr list because lambda function is an expression *)
  | Lambda of bind list * expr  (* Parameters * body, lambda must be single-lined, the value of the single expression is the return value, the return type is inferred by the compiler*)
  (*Lambda function is anomoyous but can be assigned to a func variable *)



(* statements   *)
type stmt =
    Block of stmt list
  (* assigment *)
  | Assign of id * expr
  | Bind of bind
  | BindAndAssign of bind * expr
  (* function declare  *)
  | Func of typ * id * (bind list) * (stmt list)
  | If of expr * stmt
  | Expr of expr
  | While of expr * stmt
  | For of id * expr * stmt
  | Return of expr


type program = {
  imports: import list;
  globals: stmt list
}


(* Pretty-printing functions *)
let rec string_of_bop = function
  | Add -> "+" | Mul -> "*" | Sub -> "-" | Div -> "/"
  | Equal -> "==" | Neq -> "!=" | Less -> "<"
  | And -> "&&" | Or -> "||" | Matmul -> "@" | Leq -> "<=" | Greater -> ">" | Geq -> ">="

let string_of_typ = function
  | P_int -> "int" | P_bool -> "bool" | P_float -> "float"
  | P_char -> "char" | P_string -> "string"
  | T_int -> "T_int" | T_bool -> "T_bool" | T_float -> "T_float"
  | T_char -> "T_char" | T_string -> "T_string"
  | Void -> "void" | Func -> "Function"

let rec string_of_expr = function
  | Noexpr -> "noexpr"
  | Int_literal i -> string_of_int i
  | Float_literal f -> string_of_float f
  | Bool_literal b -> string_of_bool b
  | Char_literal c -> Printf.sprintf "'%c'" c
  | String_literal s -> Printf.sprintf "\"%s\"" s
  | Tensor es -> Printf.sprintf "Tensor(%s)" (String.concat ", " (List.map string_of_expr es))
  | Id id -> id
  | Binop (e1, op, e2) -> Printf.sprintf "(%s %s %s)" (string_of_expr e1) (string_of_bop op) (string_of_expr e2)
  | Call (expr, es) -> Printf.sprintf "%s(%s)" (string_of_expr expr) (String.concat ", " (List.map string_of_expr es))
  | Lambda (bs, e) -> Printf.sprintf "lambda %s -> %s" (String.concat ", " (List.map (fun (t, id) -> Printf.sprintf "%s %s" (string_of_typ t) id) bs)) (string_of_expr e)

let rec string_of_stmt = function
  | Assign (id, e) -> Printf.sprintf "%s = %s;" id (string_of_expr e)
  | BindAndAssign ((t, id), e) -> Printf.sprintf "%s %s = %s;" (string_of_typ t) id (string_of_expr e)
  | Func (t, id, bs, stmts) ->
      let formals = String.concat ", " (List.map (fun (ty, id) -> Printf.sprintf "%s %s" (string_of_typ ty) id) bs) in
      let body = String.concat "\n" (List.map string_of_stmt stmts) in
      Printf.sprintf "%s %s(%s) {\n%s\n}" (string_of_typ t) id formals body
  | If (e, s) -> Printf.sprintf "if (%s) %s" (string_of_expr e) (string_of_stmt s)
  | Expr e -> string_of_expr e ^ ";"
  | While (e, s) -> Printf.sprintf "while (%s) %s" (string_of_expr e) (string_of_stmt s)
  | For (id,e, s) -> Printf.sprintf "for (%s in %s) %s" (id) (string_of_expr e) (string_of_stmt s)
  | Return e -> Printf.sprintf "return %s;" (string_of_expr e)
  | Bind (t,id) ->  Printf.sprintf "%s %s;" (string_of_typ t) id
  | Block (stmts) -> let body = "{" ^ String.concat "\n" (List.map string_of_stmt stmts) ^ "}" in
  Printf.sprintf "%s" body


let string_of_import (Import (m, i)) =
  Printf.sprintf "from %s import %s" m i

let string_of_program { imports; globals } =
  let imports_str = String.concat "\n" (List.map string_of_import imports) in
  let globals_str = String.concat "\n" (List.map string_of_stmt globals) in
  Printf.sprintf "%s\n\n%s" imports_str globals_str

