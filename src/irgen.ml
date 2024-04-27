open Llvm
open Sast
module L = Llvm
module A = Ast
module StringMap = Map.Make (String)

let context = L.global_context ()
let the_module = L.create_module context "hython"

let llvm_type = function
  | A.P_int | A.T_int -> L.i32_type context
  | A.P_float | A.T_float -> L.float_type context
  | A.P_bool | A.T_bool -> L.i1_type context
  | A.P_char | A.T_char -> L.i8_type context
  | A.P_string | A.T_string -> L.pointer_type (L.i8_type context)
  | A.Void -> L.void_type context
  | _ -> failwith "Unsupported type"

let lookup n map =
  try StringMap.find n map with Not_found -> StringMap.find n map

let rec build_expr func_map var_map builder ((_, e) : sexpr) : L.llvalue =
  match e with
  | SInt_literal i -> L.const_int (L.i32_type context) i
  | SFloat_literal f -> L.const_float (L.float_type context) f
  | SBool_literal b -> L.const_int (L.i1_type context) (if b then 1 else 0)
  | SChar_literal c -> L.const_int (L.i8_type context) (Char.code c)
  | SString_literal s -> L.build_global_stringptr s "tmp" builder
  | SId s -> L.build_load (lookup s var_map) s builder
  | SBinop (e1, op, e2) ->
      let e1' = build_expr func_map var_map builder e1
      and e2' = build_expr func_map var_map builder e2 in
      (match op with
      | A.Add -> L.build_add
      | A.Sub -> L.build_sub
      | A.Mul -> L.build_mul
      | A.Div -> L.build_sdiv
      | _ -> failwith "Operator not supported")
        e1' e2' "tmp" builder
  | SCall (id, args) ->
      let fdef, fdecl = StringMap.find id func_map in
      let llargs =
        List.map (fun arg -> build_expr func_map var_map builder arg) args
      in
      let result = id ^ "_result" in
      L.build_call fdef (Array.of_list llargs) result builder
  | _ -> failwith "Expression not supported"

let rec build_stmt func_map var_map builder stmt =
  match stmt with
  | SExpr e ->
      ignore (build_expr func_map var_map builder e);
      (func_map, var_map, builder)
  | SFunc fdecl ->
      let new_func_map, new_var_map, new_builder =
        build_function func_map var_map fdecl builder
      in
      (new_func_map, new_var_map, new_builder)
  | SBind (typ, id) ->
      let var = L.build_alloca (llvm_type typ) id builder in
      let new_var_map = StringMap.add id var var_map in
      (func_map, new_var_map, builder)
  | SAssign (id, e) ->
      let e' = build_expr func_map var_map builder e in
      ignore (L.build_store e' (lookup id var_map) builder);
      (func_map, var_map, builder)

and build_function func_map var_map fdecl builder =
  let name = fdecl.sfname
  and formal_types =
    Array.of_list (List.map (fun (t, _) -> llvm_type t) fdecl.sparams)
  in
  let ftype = L.function_type (llvm_type fdecl.sret_type) formal_types in
  let the_function = L.define_function name ftype the_module in
  let new_builder = L.builder_at_end context (L.entry_block the_function) in
  let new_var_map =
    let add_formal acc_mp (typ, id) p =
      L.set_value_name id p;
      let local = L.build_alloca (llvm_type typ) id builder in
      ignore (L.build_store p local builder);
      StringMap.add id local acc_mp
    in
    List.fold_left2 add_formal var_map fdecl.sparams
      (Array.to_list (L.params the_function))
  in
  let new_func_map = StringMap.add name (the_function, fdecl) func_map in

  (* build fdecl.sbody *)
  let updated_func_map, updated_var_map, final_builder =
    build_stmt_list new_func_map new_var_map new_builder fdecl.sbody
  in
  (new_func_map, var_map, final_builder)

and build_stmt_list func_map var_map builder stmts =
  List.fold_left
    (fun (f_map, v_map, bldr) stmt -> build_stmt f_map v_map bldr stmt)
    (func_map, var_map, builder)
    stmts

let translate program =
  let builder = L.builder context in
  let global_vars = StringMap.empty in
  let func_map = StringMap.empty in
  (* Assuming main is structured correctly in your SAST *)
  let main_func =
    SFunc
      {
        sret_type = A.Void;
        sfname = "main";
        sparams = [];
        sbody = program.sglobals;
      }
  in
  let func_map, global_vars, _ =
    build_stmt func_map global_vars builder main_func
  in
  the_module
