open Ast
open Sast
module StringMap = Map.Make (String)

(* TODO: Verify a list of bindings has no duplicate names  *)
(* cannot have same var name in same scope *)

type func_signature = { ret_typ : styp; formals : (styp * id) list }

(* symbol table scope *)
type symbol_table = {
  variables : styp StringMap.t;
  functions : func_signature StringMap.t;
  parent : symbol_table option;
}

let string_of_symbol_table scope =
  let variables_str =
    StringMap.fold
      (fun name typ acc -> acc ^ name ^ " : " ^ string_of_styp typ ^ "\n")
      scope.variables ""
  in
  let functions_str =
    StringMap.fold
      (fun name signature acc ->
        let formals_str =
          List.map (fun (t, id) -> string_of_styp t ^ " " ^ id) signature.formals
          |> String.concat ", "
        in
        acc ^ name ^ " : "
        ^ string_of_styp signature.ret_typ
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

(* TODO: change!!!!!!!!!! add string char and others *)
let check_assign lvaluet rvaluet err =
  match lvaluet, rvaluet with
  | ST_int _, ST_int _
  | ST_float _, ST_float _
  | ST_char _, ST_char _
  | ST_string _, ST_string _
  | ST_bool _, ST_bool _ -> rvaluet  (* Allow tensor assignments regardless of shape *)
  | _ ->
      if lvaluet = rvaluet then lvaluet
      else raise (Failure err)
let convert_typ_to_styp (typ : Ast.typ) (shape : int list) : Sast.styp =
  match typ with
  | Ast.P_int -> Sast.SP_int
  | Ast.P_bool -> Sast.SP_bool
  | Ast.P_float -> Sast.SP_float
  | Ast.P_char -> Sast.SP_char
  | Ast.P_string -> Sast.SP_string
  | Ast.T_int -> Sast.ST_int shape  (* Using provided shape for tensor types *)
  | Ast.T_bool -> Sast.ST_bool shape
  | Ast.T_float -> Sast.ST_float shape
  | Ast.T_char -> Sast.ST_char shape
  | Ast.T_string -> Sast.ST_string shape
  | Ast.Void -> Sast.SVoid
  | Ast.Func -> Sast.SFunc

let rec check_expr scope expr =
  match expr with
  (* TODO: Noexpr *)
  | Int_literal l -> (SP_int, SInt_literal l)
  | Float_literal l -> (SP_float, SFloat_literal l)
  | Bool_literal l -> (SP_bool, SBool_literal l)
  | Char_literal l -> (SP_char, SChar_literal l)
  | String_literal l -> (SP_string, SString_literal l)
  | Id var ->
    let styp = find_variable scope var in
    (styp, SId var)
  | Binop (e1, op, e2) ->
      let _ = string_of_expr e2 in
      let t1, checked_e1 = check_expr scope e1 in
      let t2, checked_e2 = check_expr scope e2 in
      let ret_typ =
        match op with
        | Add | Sub | Mul | Div -> (
            match (t1, t2) with
            | SP_int, SP_int -> SP_int
            | SP_float, SP_float -> SP_float
            | _, _ ->
                raise
                  (Failure
                     ("Invalid operands for binary operator " ^ string_of_bop op))
            )
        | Equal | Neq | Less | Leq | Greater | Geq -> (
            match (t1, t2) with
            | SP_int, SP_int -> SP_bool
            | SP_float, SP_float -> SP_bool
            | SP_char, SP_char -> SP_bool
            | SP_string, SP_string -> SP_bool
            | SP_bool, SP_bool -> SP_bool
            | _, _ ->
                raise
                  (Failure
                     ("Invalid operands for binary operator " ^ string_of_bop op))
            )
        (* TODO: Fix And and Or *)
        | And | Or -> (
            match (t1, t2) with
            | SP_bool, SP_bool -> SP_bool
            | _ ->
                raise
                  (Failure
                     ("Invalid operands for binary operator " ^ string_of_bop op))
            )
        (* TODO: Fix Matmul after Tensor is implemented !!!!!!!!!!!!!!! *)
        | Matmul -> (
          let can_multiply (shape1 : int list) (shape2 : int list) : int list option =
            (* No need to reverse the lists initially *)
            match shape1, shape2 with
            | [], _ | _, [] -> None  (* Check if either shape list is empty, which means invalid input *)
            | _ ->
              let last_dim_shape1 = List.hd (List.rev shape1)  (* Gets the last dimension of shape1 *)
              and first_dim_shape2 = List.hd shape2            (* Gets the first dimension of shape2 *)
              in
              if last_dim_shape1 = first_dim_shape2 then  (* Correct check for matrix multiplication compatibility *)
                let shape1_no_last = List.rev (List.tl (List.rev shape1))  (* Remove the last dimension from shape1 *)
                and shape2_no_first = List.tl shape2                       (* Remove the first dimension from shape2 *)
                in
                Some (shape1_no_last @ shape2_no_first)  (* Append the remaining dimensions *)
              else
                None  (* Returns None if dimensions don't match for multiplication *)
            in
            match (t1, t2) with
            | ST_float shape1, ST_float shape2 ->
              (match can_multiply shape1 shape2 with
               | Some result_shape -> ST_float result_shape
               | None -> raise (Failure ("Cannot multiply tensors with shapes " ^ (String.concat ", " (List.map string_of_int shape1)) ^ " and " ^ (String.concat ", " (List.map string_of_int shape2)))))
            | ST_int shape1, ST_int shape2 ->
                (match can_multiply shape1 shape2 with
                 | Some result_shape -> ST_int result_shape
                 | None -> raise (Failure ("Cannot multiply tensors with shapes " ^ (String.concat ", " (List.map string_of_int shape1)) ^ " and " ^ (String.concat ", " (List.map string_of_int shape2)))))
            | _ -> raise (Failure ("Invalid operands for binary operator " ^ string_of_bop Matmul ^ ". Only floating-point tensors supported."))
            )
      in
      (ret_typ, SBinop ((t1, checked_e1), op, (t2, checked_e2)))
  | Call (id, exprs) ->
      (* TODO: check function formals *)
      (* TODO: Fix scope for Call and verify correct *)
      let func_signature = find_function scope id in
      let sexprs = List.map (check_expr scope) exprs in
      let typs, _ = List.split sexprs in
      (* check if the types match the function formals *)
      let formals_typ = List.map fst func_signature.formals in
      (* TODO: change check for tensors tensors of all shape should be accepted*)
      let check_type_pairs expected found =
        if expected = found then ()
        else
          raise
            (Failure
               ("Type mismatch: expected " ^ string_of_styp expected ^ ", found "
              ^ string_of_styp found))
      in
      List.iter2 check_type_pairs formals_typ typs;
      (func_signature.ret_typ, SCall (id, sexprs))
  | Tensor exprs -> (*input: expr list*)
      let scalar_to_ast_tensor_type = function
      | SP_int -> T_int
      | SP_float -> T_float
      | SP_char -> T_char
      | SP_string -> T_string
      | SP_bool -> T_bool 
      | ST_int _ -> T_int    (* Ignore shape information *)
      | ST_float _ -> T_float
      | ST_char _ -> T_char
      | ST_string _ -> T_string
      | ST_bool _ -> T_bool
      | typ -> raise (Failure ("No tensor type equivalent for " ^ string_of_styp typ))
      in
      let infer_type sexprs= (* take the sexprs of every element as a list *)
        match sexprs with
        | [] -> raise (Failure "Empty tensor encountered")
        | (styp, _) :: _ as typed_exprs -> (* extract the type of the first expression*)
            let base_type = scalar_to_ast_tensor_type styp in  (* convert primary type to tensor type if the first expression is a scalar *)
            if List.for_all (fun (t, _) -> scalar_to_ast_tensor_type t = base_type) typed_exprs then (* check whether the rest of the expressions has the same tensor type as the first element *)
              base_type
            else
              raise (Failure "Mixed types within tensor")
      in
      let rec calculate_tensor_shape scope sexprs =
        match sexprs with
        | [] -> []
        | _ ->
            let shapes = List.map (fun (_, sx) ->
                match sx with
                | STensor(_, shape) -> shape  (* Already calculated sub-shape *)
                | SId id ->  (* Fetch shape from the variable in the scope *)
                    (match find_variable scope id with
                     | ST_int shape | ST_float shape | ST_char shape | ST_string shape | ST_bool shape -> shape
                     | _ -> [])
                | SInt_literal _ | SFloat_literal _ | SChar_literal _ | SString_literal _ | SBool_literal _ -> []  (* Treat scalars as 1D tensors *)
                | _ -> raise (Failure "Unsupported tensor element type")
            ) sexprs in
            let common_shape = List.hd shapes in
            if List.for_all (fun shape -> shape = common_shape) shapes then
                (List.length sexprs) :: common_shape
            else
                raise (Failure "Sub-tensor shapes are not uniform")
        in
      let typed_exprs = List.map (check_expr scope) exprs in (* get sexpr for each expr in Tensor*)
      let tensor_type = infer_type typed_exprs in
      let shape = calculate_tensor_shape scope typed_exprs in
      (*let shape = [0;0] in*)
      (convert_typ_to_styp tensor_type shape, STensor(typed_exprs, shape))
(* TODO: Tensor *)
(* TODO: Lambda *)

(* check if the expr is bool type  *)
let check_bool_expr scope e =
  let t, e' = check_expr scope e in
  match t with
  | SP_bool -> (t, e')
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
        let styp = convert_typ_to_styp typ [] in  
      let new_scope = add_variable id styp scope in
      (new_scope, SBind (typ, id))  
  | Assign (id, e) ->
      let variable_typ = find_variable scope id in
      let e_typ, sx = check_expr scope e in
      let err =
        "left type:" ^ string_of_styp variable_typ ^ " Right type: "
        ^ string_of_styp e_typ ^ " Variable name:" ^ id
      in
      let updated_variable_typ = check_assign variable_typ e_typ err in
      let new_scope = add_variable id updated_variable_typ scope in
      (new_scope, SAssign (id, (e_typ, sx)))
  | BindAndAssign ((typ, id), e) ->
      if StringMap.mem id scope.variables then
        raise (Failure ("Already decalered: " ^ id ^ " , in this scope"))
      else
        let styp = convert_typ_to_styp typ [] in
        let new_scope = add_variable id styp scope in
        let e_typ, sx = check_expr new_scope e in
        let err =
          "left type:" ^ string_of_styp styp ^ " Right type: "
          ^ string_of_styp e_typ ^ " Variable name:" ^ id
        in
        let updated_variable_typ = check_assign styp e_typ err in
        let new_scope = add_variable id updated_variable_typ scope in
        (new_scope, SBindAndAssign ((typ, id), (e_typ, sx)))
  | Func { ret_type = typ; fname = id; params = binds; body = stmts } ->
      if id == "print" then
        raise (Failure "Cannot declare function named print");
      if StringMap.mem id scope.functions then
        raise (Failure ("Already decalered: " ^ id ^ " , in this scope"))
      else
        let styp = convert_typ_to_styp typ [] in  (* Convert Ast.typ to Sast.styp *)
        let converted_formals = List.map (fun (t, i) -> (convert_typ_to_styp t [], i)) binds in 
        let func_signature = { ret_typ = styp; formals = converted_formals } in
        let new_function_scope = create_scope (Some scope) in
        let scope_with_formals =
          List.fold_left
            (fun acc_scope (typ, id) -> add_variable id typ acc_scope)
            new_function_scope converted_formals
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
        let new_scope = add_function id styp converted_formals scope in
        ( new_scope,
          SFunc
            {
              sret_type = styp;
              sfname = id;
              sparams = binds;
              sbody = check_function_statements;
            } )
  | While (cond_expr, loop_stmt) ->
      let cond_type, checked_cond = check_expr scope cond_expr in
      if cond_type != SP_bool then
        raise (Failure "While statement contains non-boolean value");

      let new_scope = create_scope (Some scope) in
      let _, checked_loop_stmt = check_statement new_scope loop_stmt in
      (scope, SWhile ((cond_type, checked_cond), checked_loop_stmt))
  | For (iterator, start_expr, loop_stmt) ->
      let iterator_type = find_variable_current_scope scope iterator in
      let start_type, checked_start_expr = check_expr scope start_expr in
      if iterator_type != SP_int || start_type != SP_int then
        raise (Failure "For loop has to be integer ranges");

      let loop_scope = create_scope (Some scope) in
      let loop_scope_with_iterator = add_variable iterator SP_int loop_scope in
      let _, checked_loop_stmt =
        check_statement loop_scope_with_iterator loop_stmt
      in
      (scope, SFor (iterator, (SP_int, checked_start_expr), checked_loop_stmt))
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
      { ret_typ = SP_int; formals = [ (SP_int, "x") ] }
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
