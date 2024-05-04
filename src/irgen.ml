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

let str_of_terminator builder =
  let teminator_or_not = L.block_terminator (L.insertion_block builder) in
  match teminator_or_not with
  | Some terminator ->
      "Builder terminal is:--- '" ^ L.string_of_llvalue terminator ^ "---'\n"
  | _ -> "Builder terminal is: None\n"

let str_of_block block = L.string_of_llvalue (L.value_of_block block)

(* stack for build if else  *)
(* let end_block_stack = ref []
   let push_end_block block = end_block_stack := block :: !end_block_stack

   let pop_end_block () =
     match !end_block_stack with
     | [] -> failwith "No current end block available"
     | hd :: tl ->
         end_block_stack := tl;
         hd

   (* peek *)
   let current_end_block () =
     match !end_block_stack with
     | [] -> failwith "No current end block available"
     | hd :: _ -> hd

   let print_end_block_stack () =
     Printf.printf "Current End Block Stack (size %d):\n"
       (List.length !end_block_stack);
     match !end_block_stack with
     | [] -> Printf.printf "The stack is empty.\n"
     | _ ->
         List.iter
           (fun block -> Printf.printf "Block: %s\n" (str_of_block block))
           !end_block_stack *)

(* some helper functions  *)
let prepare_builder builder =
  let current_block = L.insertion_block builder in
  match L.block_terminator current_block with
  | None ->
      L.position_at_end current_block
        builder (* No terminator, position at the end of the block *)
  | Some terminator ->
      L.position_before terminator
        builder (* Position builder before the terminator *)

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

let add_terminal builder instr =
  match L.block_terminator (L.insertion_block builder) with
  | Some _ -> ()
  | None -> ignore (instr builder)

let lookup n map =
  try StringMap.find n map with Not_found -> StringMap.find n map

let rec build_expr func_map var_map builder sexpr : L.llvalue =
  prepare_builder builder;
  let _, e = sexpr in
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

(* build the statement, if control instr, use it as the terminator instr *)
let rec build_stmt func_map var_map builder stmt
    (control_instr_opt : (L.llbuilder -> L.llvalue) option) =
  (* need to store the prev state *)
  let old_builder = builder in
  (* Printf.printf "cur expr %s \n" (string_of_sstmt stmt); *)
  match stmt with
  | SExpr e ->
      (* Printf.printf "terminator %s \n" (str_of_terminator builder); *)
      ignore (build_expr func_map var_map builder e);
      (func_map, var_map, builder)
  | SFunc fdecl ->
      let new_func_map, new_var_map, new_builder =
        build_function func_map var_map fdecl builder
      in
      (* function terminator *)
      add_terminal new_builder (L.build_ret (L.const_int (llvm_type A.P_int) 1));
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
  | SIfElse (pred, then_stmt, else_stmt) ->
      let bool_val = build_expr func_map var_map builder pred in
      (* find the parent function *)
      let the_function = L.block_parent (L.insertion_block builder) in

      (* the end if-else *)
      let end_bb = L.append_block context "if_end" the_function in
      let end_builder = L.builder_at_end context end_bb in
      let build_br_end = L.build_br end_bb in

      (* the then block *)
      let then_bb = L.append_block context "then" the_function in
      let then_builder = L.builder_at_end context then_bb in
      let _, _, then_builder_new =
        build_stmt func_map var_map then_builder then_stmt (Some build_br_end)
      in

      (* the else block *)
      let else_bb = L.append_block context "else" the_function in
      let else_builder = L.builder_at_end context else_bb in
      let _, _, else_builder_new =
        build_stmt func_map var_map else_builder else_stmt (Some build_br_end)
      in

      (* Printf.printf "else_builder is: %s\n" (str_of_terminator else_builder); *)

      (* ignore (build_stmt func_map var_map else_builder else_stmt); *)

      (* Adds does not already have a terminator *)

      (* add_terminal (L.builder_at_end context then_bb) build_br_end;
         add_terminal (L.builder_at_end context else_bb) build_br_end; *)
      (* Printf.printf "then_builder_new is: %s\n" (str_of_terminator then_builder_new); *)
      (* Printf.printf "else_builder_new is: %s\n" (str_of_terminator else_builder_new); *)
      add_terminal then_builder_new build_br_end;
      add_terminal else_builder_new build_br_end;
      (match control_instr_opt with
      | Some control_instr -> add_terminal end_builder control_instr
      | None -> ());

      ignore (L.build_cond_br bool_val then_bb else_bb builder);

      (* add_terminal
         (L.builder_at_end context end_bb)
         (L.build_ret (L.const_int (llvm_type A.P_int) 0)); *)

      (* Printf.printf "then_builder is: %s\n" (str_of_terminator then_builder);
         Printf.printf "pred is: %s\n" (string_of_sexpr pred);
         Printf.printf "else_builder is: %s\n" (str_of_terminator else_builder);
         Printf.printf "Cur_end_blcok is: %s\n" (str_of_block end_bb); *)

      (* print_end_block_stack (); *)
      (* push the end bb  *)
      (* push_end_block end_bb;
         print_end_block_stack (); *)

      (* print_end_block_stack (); *)
      (* Move the builder to the end block *)
      let end_builder = L.builder_at_end context end_bb in
      (* cur end_builder terminator is the previous terminator  *)
      (func_map, var_map, end_builder)
  | SBlock stmts ->
      let new_func_map, new_var_map, new_builder =
        build_stmt_list func_map var_map builder stmts control_instr_opt
      in
      (* add_terminal builder (L.build_ret (L.const_int (llvm_type A.P_int) 0)); *)
      (new_func_map, new_var_map, old_builder)
      (* (new_func_map, new_var_map, new_builder) *)
  | _ -> failwith "error"

and build_function func_map var_map fdecl builder =
  let name = fdecl.sfname in
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
    build_stmt_list new_func_map new_var_map new_builder fdecl.sbody None
  in
  (new_func_map, var_map, final_builder)

and build_stmt_list func_map var_map builder stmts control_instr_opt =
  List.fold_left
    (fun (f_map, v_map, bldr) stmt ->
      build_stmt f_map v_map bldr stmt control_instr_opt)
    (func_map, var_map, builder)
    stmts

let translate program =
  let builder = L.builder context in
  let global_vars = StringMap.empty in
  let func_map = StringMap.empty in
  let main_func =
    SFunc
      {
        sret_type = A.P_int;
        sfname = "main";
        sparams = [];
        sbody = program.sglobals;
      }
  in
  let func_map, global_vars, _ =
    build_stmt func_map global_vars builder main_func None
  in
  the_module
