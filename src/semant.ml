open Ast
open Sast
module StringMap = Map.Make (String)

type func_signature = { ret_typ : typ; formals : (typ * id) list }

(* symbol table scope *)
type symbol_table = {
  (* the table only hold the typ types *)
  variables : typ StringMap.t;
  functions : func_signature StringMap.t;
  parent : symbol_table option;
}

let create_scope parent =
  { variables = StringMap.empty; functions = StringMap.empty; parent }

let add_variable name typ scope =
  {
    variables = StringMap.add name typ scope.variables;
    functions = scope.functions;
    parent = scope.parent;
  }

let add_function name rtyp formals scope =
  let func_signature = { ret_typ = rtyp; formals } in
  {
    variables = scope.variables;
    functions = StringMap.add name func_signature scope.functions;
    parent = scope.parent;
  }

let rec find_variable (scope : symbol_table) name =
  try StringMap.find name scope.variables
  with Not_found -> (
    match scope.parent with
    | Some parent -> find_variable parent name
    | _ -> raise (Failure ("Variable not declared: " ^ name)))

let rec find_function (scope : symbol_table) name =
  try StringMap.find name scope.functions
  with Not_found -> (
    match scope.parent with
    | Some parent -> find_function parent name
    | _ -> raise (Failure ("Function not declared: " ^ name)))

let rec find_variable_current_scope (scope : symbol_table) name =
  try StringMap.find name scope.variables
  with Not_found ->
    raise (Failure ("Variable not declared in the current scope: " ^ name))

let check_assign lvaluet rvaluet err =
  if lvaluet = rvaluet then lvaluet else raise (Failure err)

let rec check_expr scope expr =
  match expr with
  | Int_literal l -> (P_int, SInt_literal l)
  | Float_literal l -> (P_float, SFloat_literal l)
  | Bool_literal l -> (P_bool, SBool_literal l)
  | Char_literal l -> (P_char, SChar_literal l)
  | String_literal l -> (P_string, SString_literal l)
  | Id var ->
      let typ = find_variable scope var in
      (typ, SId var)
  | Call (id, exprs) ->
      let func_signature = find_function scope id in
      let sexprs = List.map (check_expr scope) exprs in
      let typs, _ = List.split sexprs in
      (* check if the types match the function formals *)
      let formals_typ = List.map fst func_signature.formals in
      let check_type_pairs expected found =
        if expected = found then ()
        else
          raise
            (Failure
               ("Type mismatch: expected " ^ string_of_typ expected ^ ", found "
              ^ string_of_typ found))
      in
      List.iter2 check_type_pairs formals_typ typs;
      (func_signature.ret_typ, SCall (id, sexprs))

(* TODO: check stmt, return (scope, SAST) *)
let rec check_statement (scope : symbol_table) (statement : stmt) =
  match statement with
  | Expr expr ->
      let sexpr = check_expr scope expr in
      (scope, SExpr sexpr)
  | Block stmts ->
      let new_scope = create_scope (Some scope) in
      let sstmts = List.map (check_statement new_scope) stmts in
      (scope, SBlock (List.map snd sstmts))
  | Bind (typ, id) ->
      if StringMap.mem id scope.variables then
        raise (Failure ("Already decalered: " ^ id ^ " , in this scope"))
      else
        let new_scope = add_variable id typ scope in
        (* raise
           (Failure
              ("Already decalered: "
              ^ string_of_bool (StringMap.mem id new_scope.variables)
              ^ " , in this scope")); *)
        (new_scope, SBind (typ, id))
  | Assign (id, e) ->
      (* find the variable first *)
      let variable_typ = find_variable scope id in
      let e_typ, sx = check_expr scope e in
      let err =
        "left type:" ^ string_of_typ variable_typ ^ " Right type: "
        ^ string_of_typ e_typ ^ " Variable name:" ^ id
      in
      check_assign variable_typ e_typ err;
      (scope, SAssign (id, (e_typ, sx)))
  | BindAndAssign ((typ, id), e) ->
      if StringMap.mem id scope.variables then
        raise (Failure ("Already decalered: " ^ id ^ " , in this scope"))
      else
        let new_scope = add_variable id typ scope in
        let e_typ, sx = check_expr scope e in
        let err =
          "left type:" ^ string_of_typ typ ^ " Right type: "
          ^ string_of_typ e_typ ^ " Variable name:" ^ id
        in
        check_assign typ e_typ err;
        (scope, SBindAndAssign ((typ, id), (e_typ, sx)))
  | Func (typ, id, binds, stmts) ->
      (* duplicate function decals *)
      if StringMap.mem id scope.functions then
        raise (Failure ("Already decalered: " ^ id ^ " , in this scope"))
      else
        let func_signature = { ret_typ = typ; formals = binds } in
        (* new function scope  *)
        let new_function_scope = create_scope (Some scope) in
        (* then add binds to this function scope  *)
        let scope_with_formals =
          List.fold_left
            (fun acc_scope (typ, id) -> add_variable id typ acc_scope)
            new_function_scope binds
        in
        (* then check the stmts of this function  *)
        (* only extract the snd sstmt  *)
        let check_function_statements =
          List.map
            (fun stmt -> snd (check_statement scope_with_formals stmt))
            stmts
        in
        (* add the function to the new symbol table *)
        let new_scope = add_function id typ binds scope in
        (new_scope, SFunc (typ, id, binds, check_function_statements))
  | x -> failwith ("Statement type not handled yet: " ^ string_of_stmt x)
(* Extend as needed *)

(* semantic checking of ast, return Sast if success *)
(* globals are list of statements  *)
(* function and variable share the same name space under same scope *)

let rec check_statements scope statments sstatments =
  match statments with
  | [] -> sstatments
  | h :: l ->
      let new_scope, sstmt = check_statement scope h in
      let new_sstmts = sstatments @ [ sstmt ] in
      check_statements new_scope l new_sstmts

(* TODO: Verify a list of bindings has no duplicate names  *)
(* cannot have same var name in same scope *)

(* TODO: check tensor *)

(* TODO: check lambda *)

(* TODO: check Call *)

(* TODO: Add Function name to symbol table *)

(* TODO: Find Function name in symbol table *)

(* TODO: check Block *)

(* TODO: check Assign *)

(* TODO: check Binds *)

(* TODO: check BindAndAssign *)

(* TODO: check function formals *)

(* TODO: check If *)
let check_if_statement cond_expr then_stmt else_stmt scope =
  let cond_type, checked_cond = check_expr scope cond_expr in
  if cond_type != P_bool then
    raise (Failure "If statement contains non-boolean value");
  let _, checked_then = check_statement scope then_stmt in
  let _, checked_else = check_statement scope else_stmt in
  P_void, SIf (checked_cond, checked_then, checked_else)

(* TODO: check While *)
let check_while_statement cond_expr loop_stmt scope =
  let cond_type, checked_cond = check_expr scope cond_expr in
  if cond_type != P_bool then
    raise (Failure "While statement contains non-boolean value");
  
  let new_scope = create_scope (Some scope)
  in
  let _, checked_loop_stmt = check_statement new_scope loop_stmt in
  P_void, SWhile (checked_cond, checked_loop_stmt)

(* TODO: check For *)
let check_for_statement iterator start_expr end_expr loop_stmt scope =
  let start_type, checked_start = check_expr scope start_expr in
  let end_type, checked_end = check_expr scope end_expr in

  if start_type != P_int || end_type != P_int then
    raise (Failure "For loop has to be integer ranges");
  
  let iter_scope = add_variable_check_dup iterator P_int scope in  
  let new_scope = create_scope (Some iter_scope)
  in
  let _, checked_loop_stmt = check_statement new_scope loop_stmt in
  P_void, SFor (iterator, checked_start, checked_end, checked_loop_stmt)

(* TODO: check expr *)

(*TODO: find the variable in all scopes*)

(*TODO: add bind to symbol table*)

let check program =
  let imports = program.imports in
  let globals = program.globals in
  let global_scope = create_scope None in
  let sstmts = check_statements global_scope globals [] in
  { simports = [ SImport ("1", "2") ]; sglobals = sstmts }
