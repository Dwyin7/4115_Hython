(* make it possible to assigned function to variables (parameter types * return type) *)
open Ast

type simport = SImport of id * id

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
  | SCall of id * sexpr list
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

type sprogram = { simports : simport list; sglobals : sstmt list }

(* Pretty-printing functions *)

(* Pretty print binary operators *)

(* Pretty print expressions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ ") "
  ^
  match e with
  | SNoexpr -> "noexpr"
  | SInt_literal l -> string_of_int l
  | SFloat_literal l -> string_of_float l
  | SBool_literal true -> "true"
  | SBool_literal false -> "false"
  | SChar_literal c -> "'" ^ Char.escaped c ^ "'"
  | SString_literal s -> "\"" ^ s ^ "\""
  | STensor es -> "[" ^ String.concat ", " (List.map string_of_sexpr es) ^ "]"
  | SId s -> s
  | SBinop (e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_bop o ^ " " ^ string_of_sexpr e2
  | SCall (id, args) ->
      id ^ "(" ^ String.concat ", " (List.map string_of_sexpr args) ^ ")"
  | SLambda (params, body) ->
      "lambda("
      ^ String.concat ", " (List.map string_of_bind params)
      ^ "). " ^ string_of_sexpr body

and string_of_bind (t, id) = string_of_typ t ^ " " ^ id

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
  | SFunc (rt, id, params, body) ->
      "func " ^ string_of_typ rt ^ " " ^ id ^ "("
      ^ String.concat ", " (List.map string_of_bind params)
      ^ ") "
      ^ string_of_sstmt (SBlock body)
  | SIf (e, s) -> "if " ^ string_of_sexpr e ^ " " ^ string_of_sstmt s
  | SExpr e -> string_of_sexpr e
  | SWhile (e, s) -> "while " ^ string_of_sexpr e ^ " " ^ string_of_sstmt s
  | SFor (id, e, s) ->
      "for " ^ id ^ " in " ^ string_of_sexpr e ^ " " ^ string_of_sstmt s
  | SReturn e -> "return " ^ string_of_sexpr e

(* Pretty print the program *)
let string_of_sprogram { simports; sglobals } =
  let string_of_import (SImport (modul, id)) =
    "import " ^ modul ^ " as " ^ id
  in
  String.concat "\n" (List.map string_of_import simports)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_sstmt sglobals)
