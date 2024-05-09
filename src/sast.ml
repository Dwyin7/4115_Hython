(* make it possible to assigned function to variables (parameter types * return type) *)
open Ast

type styp =
  | SP_int
  | SP_bool
  | SP_float
  | SP_char
  | SP_string
  | ST_int of int list (* Integer tensor with shape information *)
  | ST_bool of int list (* Boolean tensor with shape information *)
  | ST_float of int list (* Floating point tensor with shape information *)
  | ST_char of int list (* Character tensor with shape information *)
  | ST_string of int list (* String tensor with shape information *)
  | SVoid
  | SFunc

type simport = SImport of id * id
type sbind = styp * id
(* expressions *)
type sexpr = styp * sx

and sx =
  | SNoexpr
  | SInt_literal of int
  | SFloat_literal of float
  | SBool_literal of bool
  | SChar_literal of char
  | SString_literal of string
  (* tensor *)
  | STensor of sexpr list * int list (* Added shape information *)
  | SId of id
  | SBinop of sexpr * bop * sexpr
  (* function call *)
  | SCall of id * sexpr list
  | STensorAccess of id * sexpr
    (*expr * expr list instead of Id * expr list because lambda function is an expression *)
  | SLambda of sbind list * sexpr

(* Parameters * body, lambda must be single-lined, the value of the single expression is the return value, the return type is inferred by the compiler*)
(*Lambda function is anomoyous but can be assigned to a func variable *)

(* statements   *)
type sstmt =
  | SBlock of sstmt list
  (* assigment *)
  | SAssign of id * sexpr
  | SBind of sbind
  | SBindAndAssign of sbind * sexpr
  (* function declare  *)
  | SFunc of sfunc_decl
  | SIf of sexpr * sstmt
  | SIfElse of sexpr * sstmt * sstmt
  | SExpr of sexpr
  | SWhile of sexpr * sstmt
  | SFor of id * sexpr * sstmt
  | SReturn of sexpr

and sfunc_decl = {
  sret_type : styp;
  sfname : id;
  sparams : sbind list;
  sbody : sstmt list;
}

type sprogram = { simports : simport list; sglobals : sstmt list }

(* Pretty-printing functions *)
let string_of_styp = function
  | SP_int -> "int"
  | SP_bool -> "bool"
  | SP_float -> "float"
  | SP_char -> "char"
  | SP_string -> "string"
  | ST_int dims ->
      "int tensor[" ^ String.concat ", " (List.map string_of_int dims) ^ "]"
  | ST_bool dims ->
      "bool tensor[" ^ String.concat ", " (List.map string_of_int dims) ^ "]"
  | ST_float dims ->
      "float tensor[" ^ String.concat ", " (List.map string_of_int dims) ^ "]"
  | ST_char dims ->
      "char tensor[" ^ String.concat ", " (List.map string_of_int dims) ^ "]"
  | ST_string dims ->
      "string tensor[" ^ String.concat ", " (List.map string_of_int dims) ^ "]"
  | SVoid -> "void"
  | SFunc -> "Function"

(* Pretty print binary operators *)

(* Pretty print expressions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_styp t ^ ") "
  ^
  match e with
  | SNoexpr -> "noexpr"
  | SInt_literal l -> string_of_int l
  | SFloat_literal l -> string_of_float l
  | SBool_literal true -> "true"
  | SBool_literal false -> "false"
  | SChar_literal c -> "'" ^ Char.escaped c ^ "'"
  | SString_literal s -> "\"" ^ s ^ "\""
  | STensor (es, shape) ->
      "tensor("
      ^ String.concat ", " (List.map string_of_sexpr es)
      ^ ")" ^ " shape: "
      ^ String.concat ", " (List.map string_of_int shape)
  | SId s -> s
  | SBinop (e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_bop o ^ " " ^ string_of_sexpr e2
  | SCall (id, args) ->
      id ^ "(" ^ String.concat ", " (List.map string_of_sexpr args) ^ ")"
  | STensorAccess (id, index) ->
     id ^ "[" ^ string_of_sexpr index ^ "]"
  | SLambda (params, body) ->
      "lambda("
      ^ String.concat ", " (List.map string_of_bind params)
      ^ "). " ^ string_of_sexpr body

and string_of_bind (t, id) = string_of_styp t ^ " " ^ id

(* Pretty print statements *)
let rec string_of_sstmt = function
  | SBlock stmts ->
      "{\n"
      ^ String.concat "" (List.map (fun s -> string_of_sstmt s ^ "\n") stmts)
      ^ "}"
  | SAssign (id, e) -> id ^ " = " ^ string_of_sexpr e
  | SBind (t, id) -> string_of_bind (t, id)
  | SBindAndAssign ((t, id), e) ->
      string_of_bind (t, id) ^ " = " ^ string_of_sexpr e
  | SFunc { sret_type = rt; sfname = id; sparams = params; sbody = body } ->
      "func " ^ string_of_styp rt ^ " " ^ id ^ "("
      ^ String.concat ", " (List.map string_of_bind params)
      ^ ") "
      ^ string_of_sstmt (SBlock body)
  | SIf (e, s) -> "if " ^ string_of_sexpr e ^ " " ^ string_of_sstmt s
  | SExpr e -> string_of_sexpr e
  | SWhile (e, s) -> "while " ^ string_of_sexpr e ^ " " ^ string_of_sstmt s
  | SFor (id, e, s) ->
      "for " ^ id ^ " in " ^ string_of_sexpr e ^ " " ^ string_of_sstmt s
  | SReturn e -> "return " ^ string_of_sexpr e
  | SIfElse (sexpr, sstmt1, sstmt2) ->
      "IF: " ^ string_of_sexpr sexpr ^ ", Then: " ^ string_of_sstmt sstmt1
      ^ "Else: " ^ string_of_sstmt sstmt2
  
  | _ -> failwith "not yet"

let string_of_sstmts stmts =
  "{\n"
  ^ String.concat "" (List.map (fun s -> string_of_sstmt s ^ "\n") stmts)
  ^ "}"

(* Pretty print the program *)
let string_of_sprogram { simports; sglobals } =
  let string_of_import (SImport (modul, id)) =
    "import " ^ modul ^ " as " ^ id
  in
  String.concat "\n" (List.map string_of_import simports)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_sstmt sglobals)
