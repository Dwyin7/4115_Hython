open Llvm
open Sast
module L = Llvm
module A = Ast
module StringMap = Map.Make (String)

(* define the records for both func map and var map  *)
type var_map_val = { typ : A.typ; the_var : L.llvalue }

type func_map_val = {
  fdecl : sfunc_decl;
  the_function : L.llvalue;
  snap_shot_var_map : var_map_val StringMap.t;
      (*this store the outer scope variable before func declare*)
}

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
(* helper functions for debugging *)

let str_of_var_map var_map =
  let str_list = ref [] in
  StringMap.iter
    (fun key the_var ->
      let func_str =
        "Var name: " ^ key ^ ", LLVM value: "
        ^ L.string_of_llvalue the_var.the_var
      in
      str_list := func_str :: !str_list)
    var_map;
  String.concat "\n" (List.rev !str_list)

let str_of_func_map func_map =
  let str_list = ref [] in
  StringMap.iter
    (fun key value ->
      let func_str =
        "++++ Func name: " ^ key ^ ",\n Snap shot: "
        ^ str_of_var_map value.snap_shot_var_map
        ^ " ++++\n"
        (* ^ ",\n LLVM value: "
           ^ L.string_of_llvalue value.the_function *)
      in
      str_list := func_str :: !str_list)
    func_map;
  String.concat "\n" (List.rev !str_list)

(* some helper functions  *)
let sorted_map_to_list map =
  let sorted_list =
    StringMap.fold (fun key value acc -> (key, value) :: acc) map []
  in
  List.sort (fun (k1, _) (k2, _) -> compare k1 k2) sorted_list

let sorted_map_to_key_list map =
  let tmp_list = sorted_map_to_list map in
  List.fold_left (fun acc (key, _) -> key :: acc) [] tmp_list

let ll_types_of_var_map var_map =
  let tmp_list = sorted_map_to_list var_map in

  List.fold_left
    (fun acc (_, value) ->
      (* Printf.printf "+++++++++%s" (L.string_of_lltype (llvm_type value.typ)); *)
      llvm_type value.typ :: acc)
    [] tmp_list

let binds_of_var_map var_map =
  let tmp_list = sorted_map_to_list var_map in
  List.fold_left (fun acc (key, value) -> (value.typ, key) :: acc) [] tmp_list

let print_call llval_expr builder =
  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
  let printf_t : L.lltype =
    L.var_arg_function_type (L.i32_type context)
      [| L.pointer_type (L.i8_type context) |]
  in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module
  in
  L.build_call printf_func [| int_format_str; llval_expr |] "printf" builder

let lookup n map =
  try StringMap.find n map with Not_found -> StringMap.find n map

let rec build_expr func_map var_map builder ((_, e) : sexpr) : L.llvalue =
  match e with
  | SInt_literal i -> L.const_int (L.i32_type context) i
  | SFloat_literal f -> L.const_float (L.float_type context) f
  | SBool_literal b -> L.const_int (L.i1_type context) (if b then 1 else 0)
  | SChar_literal c -> L.const_int (L.i8_type context) (Char.code c)
  | SString_literal s -> L.build_global_stringptr s "tmp" builder
  | SId s -> L.build_load (lookup s var_map).the_var s builder
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
  | SCall ("print", [ e ]) ->
      print_call (build_expr func_map var_map builder e) builder
  | SCall (id, args) ->
      let func_map_val = StringMap.find id func_map in
      (* find the snap_shot *)
      let snap_shot_var_key_lst =
        sorted_map_to_key_list func_map_val.snap_shot_var_map
      in
      (* Printf.printf "New calls snap: [";
         List.iter (fun key -> Printf.printf "(%s " key) snap_shot_var_key_lst;
         Printf.printf "]\n"; *)
      let ids =
        List.fold_right
          (fun key acc -> (A.Void, SId key) :: acc)
          snap_shot_var_key_lst []
      in
      (* Printf.printf "New calls: [";
         List.iter (fun (_, SId key) -> Printf.printf "(%s " key) ids;
         Printf.printf "]\n"; *)
      let new_args = ids @ args in
      let llargs =
        List.map (fun arg -> build_expr func_map var_map builder arg) new_args
      in
      let result = id ^ "_result" in
      L.build_call func_map_val.the_function (Array.of_list llargs) result
        builder
  | _ -> failwith "Expression not supported"

(* build the statement *)
let rec build_stmt func_map var_map builder stmt =
  (* need to store the prev state *)
  let old_builder = builder in
  match stmt with
  | SExpr e ->
      ignore (build_expr func_map var_map builder e);
      (func_map, var_map, builder)
  | SFunc fdecl ->
      let new_func_map, new_var_map, new_builder =
        build_function func_map var_map fdecl builder
      in
      (new_func_map, new_var_map, old_builder)
  | SBind (typ, id) ->
      let var = L.build_alloca (llvm_type typ) id builder in
      let var_val = { typ; the_var = var } in
      let new_var_map = StringMap.add id var_val var_map in
      (func_map, new_var_map, builder)
  | SAssign (id, e) ->
      let e' = build_expr func_map var_map builder e in
      ignore (L.build_store e' (lookup id var_map).the_var builder);
      (func_map, var_map, builder)

and build_function func_map var_map fdecl builder =
  let name = fdecl.sfname in
  (* Printf.printf "Building function name: %s\n" name; *)
  (* Printf.printf "Var map (outer scope):\n %s\n" (str_of_var_map var_map); *)
  (* Printf.printf "Current func map: %s\n" func_map;  *)
  let formal_types =
    Array.of_list (List.map (fun (t, _) -> llvm_type t) fdecl.sparams)
  in
  let outer_var_types = Array.of_list (ll_types_of_var_map var_map) in
  (* append the outer_var_types + formal_types  *)
  let new_types = Array.append outer_var_types formal_types in
  (* new parameter for this function: include the outer_var + formals  *)
  let new_params = binds_of_var_map var_map @ fdecl.sparams in
  (* Printf.printf "New params %s" (str_of_var_map var_map);
     Printf.printf "New types";
     Array.iter
       (fun item -> Printf.printf "%s\n" (L.string_of_lltype item))
       outer_var_types; *)
  let ftype = L.function_type (llvm_type fdecl.sret_type) new_types in
  (* Printf.printf "ftype:  %s\n" (string_of_lltype ftype); *)
  let the_function = L.define_function name ftype the_module in
  let new_builder = L.builder_at_end context (L.entry_block the_function) in
  let new_var_map =
    let add_formal acc_mp (typ, id) p =
      L.set_value_name id p;
      let local = L.build_alloca (llvm_type typ) id new_builder in
      let var_map_val = { typ; the_var = local } in
      ignore (L.build_store p local new_builder);
      StringMap.add id var_map_val acc_mp
    in
    List.fold_left2 add_formal var_map new_params
      (Array.to_list (L.params the_function))
  in
  (* Add this function to the function map *)
  let new_func_map =
    StringMap.add name
      { fdecl; the_function; snap_shot_var_map = var_map }
      func_map
  in

  (* Printf.printf "Cur decl: %s \n============ %s ============ \n" name
     (str_of_var_map var_map); *)

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
