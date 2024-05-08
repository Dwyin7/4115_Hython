open Ast
open Sast
module StringMap = Map.Make (String)

(* TODO: Verify a list of bindings has no duplicate names  *)
(* cannot have same var name in same scope *)

type func_signature = { ret_typ : typ; formals : (typ * id) list }

(* symbol table scope *)
type symbol_table = {
  (* the table only hold the typ types *)
  variables : typ StringMap.t;
  functions : func_signature StringMap.t;
  parent : symbol_table option;
}

let string_of_symbol_table scope =
  let variables_str =
    StringMap.fold
      (fun name typ acc -> acc ^ name ^ " : " ^ string_of_typ typ ^ "\n")
      scope.variables ""
  in
  let functions_str =
    StringMap.fold
      (fun name signature acc ->
        let formals_str =
          List.map (fun (t, id) -> string_of_typ t ^ " " ^ id) signature.formals
          |> String.concat ", "
        in
        acc ^ name ^ " : "
        ^ string_of_typ signature.ret_typ
        ^ " (" ^ formals_str ^ ")\n")
      scope.functions ""
  in
  "Variables:\n" ^ variables_str ^ "\nFunctions:\n" ^ functions_str

let create_scope parent =
  (* let init_functions = StringMap.add "print" value StringMap.empty in *)
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

(* TODO: verify if need to use scope *)
let check_assign lvaluet rvaluet err =
  if lvaluet = rvaluet then lvaluet else raise (Failure err)

let rec check_expr scope expr =
  match expr with
  (* TODO: Noexpr *)
  | Int_literal l -> (P_int, SInt_literal l)
  | Float_literal l -> (P_float, SFloat_literal l)
  | Bool_literal l -> (P_bool, SBool_literal l)
  | Char_literal l -> (P_char, SChar_literal l)
  | String_literal l -> (P_string, SString_literal l)
  | Id var ->
      let typ = find_variable scope var in
      (typ, SId var)
  | Binop (e1, op, e2) ->
      let _ = string_of_expr e2 in
      let t1, checked_e1 = check_expr scope e1 in
      let t2, checked_e2 = check_expr scope e2 in
      let ret_typ =
        match op with
        | Add | Sub | Mul | Div -> (
            match (t1, t2) with
            | P_int, P_int -> P_int
            | P_float, P_float -> P_float
            | _, _ ->
                raise
                  (Failure
                     ("Invalid operands for binary operator " ^ string_of_bop op))
            )
        | Equal | Neq | Less | Leq | Greater | Geq -> (
            match (t1, t2) with
            | P_int, P_int -> P_bool
            | P_float, P_float -> P_bool
            | P_char, P_char -> P_bool
            | P_string, P_string -> P_bool
            | P_bool, P_bool -> P_bool
            | _, _ ->
                raise
                  (Failure
                     ("Invalid operands for binary operator " ^ string_of_bop op))
            )
        (* TODO: Fix And and Or *)
        | And | Or -> (
            match (t1, t2) with
            | P_bool, P_bool -> P_bool
            | _ ->
                raise
                  (Failure
                     ("Invalid operands for binary operator " ^ string_of_bop op))
            )
        (* TODO: Fix Matmul after Tensor is implemented *)
        | Matmul -> (
            match (t1, t2) with
            | P_int, P_int -> P_bool
            | _ ->
                raise
                  (Failure
                     ("Invalid operands for binary operator " ^ string_of_bop op))
            )
      in
      (ret_typ, SBinop ((t1, checked_e1), op, (t2, checked_e2)))
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

(* TODO: Tensor *)
(* TODO: Lambda *)

(* check if the expr is bool type  *)
let check_bool_expr scope e =
  let t, e' = check_expr scope e in
  match t with
  | P_bool -> (t, e')
  | _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))

let rec check_statement (scope : symbol_table) (statement : stmt) =
  match statement with
  | Expr expr ->
      let sexpr = check_expr scope expr in
      (scope, SExpr sexpr)
  | Block stmts ->
      let _, checked_stmts =
        List.fold_left
          (fun (current_scope, checked_stmts) stmt ->
            let new_scope, checked_stmt = check_statement current_scope stmt in
            (new_scope, checked_stmts @ [ checked_stmt ]))
          (create_scope (Some scope), [])
          stmts
      in
      (scope, SBlock checked_stmts)
  | Bind (typ, id) ->
      if StringMap.mem id scope.variables then
        raise (Failure ("Already decalered: " ^ id ^ " , in this scope"))
      else
        let new_scope = add_variable id typ scope in
        (new_scope, SBind (typ, id))
  | Assign (id, e) ->
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
        let e_typ, sx = check_expr new_scope e in
        let err =
          "left type:" ^ string_of_typ typ ^ " Right type: "
          ^ string_of_typ e_typ ^ " Variable name:" ^ id
        in
        check_assign typ e_typ err;
        (new_scope, SBindAndAssign ((typ, id), (e_typ, sx)))
  | Func { ret_type = typ; fname = id; params = binds; body = stmts } ->
      if id == "print" then
        raise (Failure "Cannot declare function named print");
      if StringMap.mem id scope.functions then
        raise (Failure ("Already decalered: " ^ id ^ " , in this scope"))
      else
        let func_signature = { ret_typ = typ; formals = binds } in
        let new_function_scope = create_scope (Some scope) in
        let scope_with_formals =
          List.fold_left
            (fun acc_scope (typ, id) -> add_variable id typ acc_scope)
            new_function_scope binds
        in
        let check_function_statements =
          let _, checked_stmts =
            List.fold_left
              (fun (current_scope, checked_stmts) stmt ->
                let new_scope, checked_stmt =
                  check_statement current_scope stmt
                in
                (new_scope, checked_stmts @ [ checked_stmt ]))
              (scope_with_formals, []) stmts
          in
          checked_stmts
        in
        let new_scope = add_function id typ binds scope in
        ( new_scope,
          SFunc
            {
              sret_type = typ;
              sfname = id;
              sparams = binds;
              sbody = check_function_statements;
            } )
  | While (cond_expr, loop_stmt) ->
      let cond_type, checked_cond = check_expr scope cond_expr in
      if cond_type != P_bool then
        raise (Failure "While statement contains non-boolean value");

      let new_scope = create_scope (Some scope) in
      let _, checked_loop_stmt = check_statement new_scope loop_stmt in
      (scope, SWhile ((cond_type, checked_cond), checked_loop_stmt))
  | For (iterator, start_expr, loop_stmt) ->
      let iterator_type = find_variable_current_scope scope iterator in
      let start_type, checked_start_expr = check_expr scope start_expr in
      if iterator_type != P_int || start_type != P_int then
        raise (Failure "For loop has to be integer ranges");

      let loop_scope = create_scope (Some scope) in
      let loop_scope_with_iterator = add_variable iterator P_int loop_scope in
      let _, checked_loop_stmt =
        check_statement loop_scope_with_iterator loop_stmt
      in
      (scope, SFor (iterator, (P_int, checked_start_expr), checked_loop_stmt))
  (* TODO: If else *)
  | IfElse (e, s1, s2) ->
      ( scope,
        SIfElse
          ( check_bool_expr scope e,
            snd (check_statement scope s1),
            snd (check_statement scope s2) ) )
  (* TODO: Return *)
  (* TODO: If *)
  | x -> failwith ("Statement type not handled yet: " ^ string_of_stmt x)

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

let check program =
  let imports = program.imports in
  let globals = program.globals in
  let init_scope = create_scope None in
  let print_func =
    StringMap.add "print"
      { ret_typ = P_int; formals = [ (P_int, "x") ] }
      StringMap.empty
  in
  let global_scope =
    {
      variables = init_scope.variables;
      functions = print_func;
      parent = init_scope.parent;
    }
  in
  let sstmts = check_statements global_scope globals [] in
  { simports = [ SImport ("1", "2") ]; sglobals = sstmts }

(* Previous If implementation *)
(* let check_if_statement cond_expr then_stmt else_stmt scope =
   let cond_type, checked_cond = check_expr scope cond_expr in
   if cond_type != P_bool then
     raise (Failure "If statement contains non-boolean value");
   let _, checked_then = check_statement scope then_stmt in
   let _, checked_else = check_statement scope else_stmt in
   P_void, SIf (checked_cond, checked_then, checked_else) *)
