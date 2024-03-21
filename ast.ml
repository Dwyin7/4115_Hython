

type bop = Add | Mul | Sub | Div | Equal | Neq | Less | And | Or | Matmul

type typ = P_int | P_bool | P_float | P_char | P_string | T_int | T_bool | T_float | T_char | T_string | Void

type id = string

(* from lib_x import func_y *)
type import = Import of id * id


type bind = typ * id


type expr =
  | Noexpr
  | Int_literal of int
  | Float_literal of float
  | Bool_literal of bool
  | Char_literal of char
  | String_literal of string
  | Id of id 
  | Binop of expr * bop * expr
  (* type bind *)
  | Bind of typ * id
  (* assigment *)
  | Assign of id * expr
  | BindAndAssign of bind * expr

  (* function call *)
  | Call of id * expr list

  (* function expr  *)
  | Fname of id
  | Import of id * id
  | Formal of bind



type stmt =
  | If of expr * stmt
  | Expr of expr
  | While of expr * stmt
  | For of expr * stmt
  | Return of expr


(* type  *)


type func = {
  output_type: typ ;
  func_name: id ;
  formals: expr list;
  (* locals: bind list; *)
  body: stmt list;
  (* functions: func list; *)
}

type program = {
  imports: expr list;
  functions: func list
}




(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "&&"
  | Or -> "||"

let rec string_of_typ = function
    P_int -> "P_int"
  | P_bool -> "P_bool"
  | P_float -> "P_float"
  | P_char -> "P_char"
  | P_string -> "P_string"
  | T_int -> "T_int"
  | T_bool -> "T_bool"
  | T_float -> "T_float"
  | T_char -> "T_char"
  | T_string -> "T_string"
  | Void -> "Void"

let rec string_of_expr = function
  | Noexpr -> "Noexpr"
  | Int_literal i -> "Int(" ^ string_of_int i ^ ")"
  | Float_literal f -> "Float(" ^ string_of_float f ^ ")"
  | Bool_literal b -> "Bool(" ^ string_of_bool b ^ ")"
  | Char_literal c -> "Char('" ^ Char.escaped c ^ "')"
  | String_literal s -> "String(\"" ^ s ^ "\")"
  | Id id -> "Id(" ^ id ^ ")"
  | Binop(e1, op, e2) ->
      "Binop(" ^ string_of_expr e1 ^ ", " ^
      string_of_op op ^ ", " ^ string_of_expr e2 ^ ")"
  | Assign(id, expr) -> "Assign(" ^ id ^ ", " ^ string_of_expr expr  ^ ")"
  | Bind(typ, id) -> "Bind(" ^ string_of_typ typ ^ ", " ^ id ^ ")"
  | Call(id, exprs) ->
      "Call(" ^ id ^ ", [" ^
      String.concat ", " (List.map string_of_expr exprs) ^ "])"
  | Fname id -> "Fname(" ^ id ^ ")"
  | Import(lib, fn) -> "Import(" ^ lib ^ ", " ^ fn ^ ")"
  | Formal(bind) -> "Formal(" ^ string_of_bind bind ^ ")"
  | BindAndAssign(bind,exprs) -> "BindAndAssign (" ^ string_of_bind bind ^ ", Assign exprs " ^ string_of_expr exprs ^ ")"
and string_of_bind (t, id) =
  string_of_typ t ^ " " ^ id


let string_of_formal = function
    Formal(t, id) -> "(" ^ string_of_typ t ^ " " ^ id ^ ")"
  | _ -> "error in formal"
let string_of_id = function x -> x

let rec string_of_import = function 
  | Import(lib,"") -> "Import -> lib: " ^ lib  ^ "\n"
  | Import(lib,fn) -> "Import -> func: " ^ fn ^ " lib: "^ lib ^ "\n"
  | _ -> "error"

    
let rec string_of_stmt = function
    | If (e, s) -> "If (" ^ string_of_expr e ^ ") {\n" ^ string_of_stmt s ^ "\n}"
    | Expr e -> "Expr (" ^ string_of_expr e ^ ");"
    | While (e, s) -> "While (" ^ string_of_expr e ^ ") {\n" ^ string_of_stmt s ^ "\n}"
    | For (e, s) -> "For (" ^ string_of_expr e ^ ") {\n" ^ string_of_stmt s ^ "\n}"
    | Return (e) -> "Return (" ^ string_of_expr e ^ ")"
  
let string_of_function_decl func =
    "Function -> " ^ 
    "Output Type: " ^ string_of_typ func.output_type ^ ", " ^
    "Name: " ^ func.func_name ^ ", " ^
    "Formals: [" ^ String.concat ", " (List.map string_of_formal func.formals) ^ "],\n" ^
    "Body: [\n" ^ String.concat "\n" (List.map string_of_stmt func.body) ^ "\n]\n"
  


let string_of_program prog =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_import prog.imports) ^
  "\n" ^
  String.concat "" (List.map string_of_function_decl prog.functions) ^
  "\n"

