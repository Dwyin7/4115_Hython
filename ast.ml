

type bop = Add | Sub | Equal | Neq | Less | And | Or

type typ = P_int | P_bool | P_float | P_char | P_string | T_int | T_bool | T_float | T_char | T_string | Void

type id = string

(* from lib_x import func_y *)
type import = Import of id * id

type expr = 
  | Int_literal of int
  | Float_literal of float
  | Bool_literal of bool
  | Char_literal of char
  | String_literal of string
  | Id of id 
  | Binop of expr * bop * expr
  | Assign of id * expr

  (* function expr  *)
  | Fname of id
  | Import of id * id
  | Formal of typ * id


type stmt =
  | If of expr * stmt
  | Expr of expr
  | While of expr * stmt
  | For of expr * stmt

(* type  *)


type func = {
  output_type: typ ;
  func_name: id ;
  formals: expr list;
  (* body: ; *)
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
    Int_literal i -> "Int(" ^ string_of_int i ^ ")"
  | Float_literal f -> "Float(" ^ string_of_float f ^ ")"
  | Bool_literal b -> "Bool(" ^ string_of_bool b ^ ")"
  | Char_literal c -> "Char('" ^ String.make 1 c ^ "')"
  | String_literal s -> "String(\"" ^ s ^ "\")"
  | Fname id -> "Function Name: " ^ id
  | Import(lib, fn) -> "Import(" ^ lib ^ ", " ^ fn ^ ")"
  | Formal(t, id) -> "Formal(" ^ string_of_typ t ^ ", " ^ id ^ ")"



let string_of_formal = function
    Formal(t, id) -> "(" ^ string_of_typ t ^ " " ^ id ^ ")"
  | _ -> "error in formal"
let string_of_id = function x -> x

let rec string_of_import = function 
  | Import(lib,"") -> "Import -> lib: " ^ lib  ^ "\n"
  | Import(lib,fn) -> "Import -> func: " ^ fn ^ " lib: "^ lib ^ "\n"
  | _ -> "error"

  let string_of_function_decl func =
    "Function -> " ^ 
    "Output Type: " ^ string_of_typ func.output_type ^ ", " ^
    "Name: " ^ func.func_name ^ ", " ^
    "Formals: [" ^ String.concat ", " (List.map string_of_formal func.formals) ^ "]\n"
  

let string_of_program prog =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_import prog.imports) ^
  "\n" ^
  String.concat "" (List.map string_of_function_decl prog.functions) ^
  "\n"

