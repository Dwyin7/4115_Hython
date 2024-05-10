open Llvm
open Sast
module L = Llvm
module A = Ast
module StringMap = Map.Make (String)

(* define the records for both func map and var map  *)
type var_map_val = { typ : styp; the_var : L.llvalue }

type func_map_val = {
  fdecl : sfunc_decl;
  the_function : L.llvalue;
  snap_shot_var_map : var_map_val StringMap.t;
      (*this store the outer scope variable before func declare*)
}

let context = L.global_context ()
let the_module = L.create_module context "hython"

let llvm_type = function
  | SP_int -> L.i32_type context
  | SP_float -> L.float_type context
  | SP_bool -> L.i1_type context
  | SP_char -> L.i8_type context
  | SP_string -> L.pointer_type (L.i8_type context)
  | SVoid -> L.void_type context
  | _ -> failwith "Unsupported type"

let rec llvm_type_of_tensor context base_typ dims =
  match dims with
  | [] -> base_typ
  | dim :: rest_dims ->
      let sub_type = llvm_type_of_tensor context base_typ rest_dims in
      L.array_type sub_type dim

(* helper functions for debugging *)

let str_of_var_map_val var_map_val =
  " LLVM value: "
  ^ L.string_of_llvalue var_map_val.the_var
  ^ " typ: "
  ^ string_of_styp var_map_val.typ

let str_of_var_map var_map =
  let str_list = ref [] in
  StringMap.iter
    (fun key the_var ->
      let func_str =
        "Var name: " ^ key ^ ", LLVM value: " ^ str_of_var_map_val the_var
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

let print_call e' builder =
  let llvm_type = L.type_of e' in
  let format_str, cast_needed =
    if llvm_type == L.i32_type context then ("%d\n", false) (* Integer *)
    else if llvm_type == L.float_type context then ("%f\n", false) (* Float *)
    else if llvm_type == L.i1_type context then ("%d\n", true)
    else if llvm_type == L.i8_type context then ("%c\n", false) (* Char *)
    else if llvm_type == L.pointer_type (L.i8_type context) then ("%s\n", false)
    else failwith "Unsupported type for print"
  in
  let value_to_print =
    if cast_needed then
      L.build_intcast e' (L.i32_type context) "tmpcast" builder
    else e'
  in
  let format_str_val = L.build_global_stringptr format_str "fmt" builder in

  let printf_t : L.lltype =
    L.var_arg_function_type (L.i32_type context)
      [| L.pointer_type (L.i8_type context) |]
  in
  let printf_func : L.llvalue =
    match L.lookup_function "printf" the_module with
    | Some func -> func
    | None -> L.declare_function "printf" printf_t the_module
  in
  L.build_call printf_func [| format_str_val; value_to_print |] "printf" builder

(* let print_array_call array_llvalue array_length builder =
   let the_function = L.block_parent (L.insertion_block builder) in
   let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
   let printf_t : L.lltype =
     L.var_arg_function_type (L.i32_type context)
       [| L.pointer_type (L.i8_type context); L.i32_type context |]
   in
   let printf_func : L.llvalue =
     L.declare_function "printf" printf_t the_module
   in

   (* Create loop to iterate through the array *)
   let start_val = L.const_int (L.i32_type context) 0 in
   let end_val = L.const_int (L.i32_type context) array_length in
   let loop_cond_bb = L.append_block context "loopcond" the_function in
   let loop_body_bb = L.append_block context "loopbody" the_function in
   let after_loop_bb = L.append_block context "afterloop" the_function in

   (* Initial jump to the loop condition *)
   ignore (L.build_br loop_cond_bb builder);

   (* Condition check *)
   let cond_builder = L.builder_at_end context loop_cond_bb in
   let index_var = L.build_phi [(start_val, L.insertion_block builder)] "index" cond_builder in
   let compare = L.build_icmp L.Icmp.Slt index_var end_val "loopcond" cond_builder in
   ignore (L.build_cond_br compare loop_body_bb after_loop_bb cond_builder);

   (* Loop body *)
   let body_builder = L.builder_at_end context loop_body_bb in
   let element_ptr = L.build_gep array_llvalue [| index_var |] "element_ptr" body_builder in
   let element = L.build_load element_ptr "element" body_builder in
   ignore (L.build_call printf_func [| int_format_str; element |] "printf" body_builder);
   let next_index = L.build_add index_var (L.const_int (L.i32_type context) 1) "nextindex" body_builder in
   add_incoming (next_index, L.insertion_block body_builder) index_var;
   ignore (L.build_br loop_cond_bb body_builder);

   (* After the loop *)
   L.position_at_end after_loop_bb builder;
   L.build_ret (L.const_int (L.i32_type context) 0) builder
*)

let add_terminal builder instr =
  match L.block_terminator (L.insertion_block builder) with
  | Some _ -> ()
  (* ignore (instr builder) *)
  | None -> ignore (instr builder)

let lookup n map =
  try StringMap.find n map with Not_found -> StringMap.find n map

let rec build_expr func_map var_map builder sexpr : L.llvalue =
  prepare_builder builder;
  let rec build_tensor context builder value_typ dims init_values =
    match dims with
    | [ n ] ->
        let base_type = L.array_type value_typ n in
        L.const_array value_typ (Array.of_list init_values)
  in
  (* | n :: ns ->
     let sub_arrays = List.map (build_tensor context builder value_typ ns) init_values in
     let array_type = L.array_type (L.type_of (list.hd sub_arrays)) n in
     L.const_array (L.type_of (List.hd sub_arrays)) (Array.of_list sub_arrays) in *)
  let rec create_init_values context builder = function
    | [] -> []
    | (typ, SInt_literal i) :: rest ->
        L.const_int (L.i32_type context) i
        :: create_init_values context builder rest
    | (typ, SFloat_literal f) :: rest ->
        L.const_float (L.float_type context) f
        :: create_init_values context builder rest
  in
  let infer_type sexprs =
    (* take the sexprs of every element as a list, only check type *)
    match sexprs with
    | (styp, _) :: _ as typed_exprs ->
        (* extract the type of the first expression*)
        llvm_type styp
  in
  let get_type_of_expr var_map expr =
    match expr with
    | _, SId s -> (
        match StringMap.find_opt s var_map with Some { typ; _ } -> typ)
    | _, SInt_literal _ -> SP_int
    | _, STensor (_, shape) ->
        let shape_str = String.concat ", " (List.map string_of_int shape) in
        Printf.printf "Shape: [%s]\n" shape_str;
        ST_int shape
    | _ -> failwith "Expression type not supported for operation"
  in
  let build_tensor_addition context builder tensor1 tensor2 result_tensor etyp
      dims =
    (* let shape_str = String.concat ", " (List.map string_of_int dims) in
       Printf.printf "Shape: [%s]\n" shape_str; *)
    let size = List.fold_left ( * ) 1 dims in
    for i = 0 to size - 1 do
      let index =
        [
          L.const_int (L.i32_type context) 0; L.const_int (L.i32_type context) i;
        ]
      in
      let elem1_ptr =
        L.build_gep tensor1 (Array.of_list index) "elem1_ptr" builder
      in
      let elem2_ptr =
        L.build_gep tensor2 (Array.of_list index) "elem2_ptr" builder
      in

      let elem1 = L.build_load elem1_ptr "elem1" builder in
      let elem2 = L.build_load elem2_ptr "elem2" builder in

      let sum =
        match etyp with
        | SP_int -> L.build_add elem1 elem2 "sum" builder
        | _ -> failwith "Unsupported element type for tensor addition"
      in

      let result_ptr =
        L.build_gep result_tensor (Array.of_list index) "result_ptr" builder
      in
      ignore (L.build_store sum result_ptr builder)
    done
  in

  let _, e = sexpr in
  match e with
  | SInt_literal i -> L.const_int (L.i32_type context) i
  | SFloat_literal f -> L.const_float (L.float_type context) f
  | SBool_literal b -> L.const_int (L.i1_type context) (if b then 1 else 0)
  | SChar_literal c -> L.const_int (L.i8_type context) (Char.code c)
  | SString_literal s -> L.build_global_stringptr s "tmp" builder
  | SId s -> (
      let var_info = lookup s var_map in
      (* Printf.printf "var map: %s \n" (str_of_var_map_val var_info); *)
      match var_info.typ with
      (* | SP_int _ -> var_info.the_var *)
      | _ -> L.build_load (lookup s var_map).the_var s builder)
  | STensor (e, shape) ->
      let init_values = create_init_values context builder e in
      let tensor_typ = infer_type e in
      build_tensor context builder tensor_typ shape init_values
  | STensorAccess (var, ind) ->
      let var_info = lookup var var_map in
      let index = build_expr func_map var_map builder ind in
      let element_ptr =
        L.build_gep var_info.the_var
          [| L.const_int (L.i32_type context) 0; index |]
          "elem_ptr" builder
      in
      L.build_load element_ptr "elem" builder
  | SBinop (e1, op, e2) -> (
      let e1' = build_expr func_map var_map builder e1
      and e2' = build_expr func_map var_map builder e2 in
      let type_e = get_type_of_expr var_map e1 in
      match (type_e, op) with
      | SP_int, A.Add -> L.build_add e1' e2' "tmp" builder
      | ST_int shape, A.Add ->
          let elem_type = SP_int in
          let result_tensor =
            L.build_alloca
              (llvm_type_of_tensor context (llvm_type SP_int) shape)
              "result_tensor" builder
          in
          build_tensor_addition context builder e1' e2' result_tensor elem_type
            shape;
          result_tensor
      | _, A.Sub -> L.build_sub e1' e2' "tmp" builder
      | _, A.Mul -> L.build_mul e1' e2' "tmp" builder
      | _, A.Div -> L.build_sdiv e1' e2' "tmp" builder
      | _, A.Or -> L.build_or e1' e2' "tmp" builder
      | _, A.Equal -> L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
      | _, A.Neq -> L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
      | _, A.Less -> L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder
      | _, A.Leq -> L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder
      | _, A.Greater -> L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder
      | _, A.Geq -> L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder
      | _, A.And -> L.build_and e1' e2' "tmp" builder
      | _ -> failwith (string_of_sexpr sexpr))
  | SCall ("print", [ e ]) ->
      let e' = build_expr func_map var_map builder e in
      print_call e' builder
  | SCall (id, args) ->
      let func_map_val = StringMap.find id func_map in
      (* find the snap_shot *)
      let snap_shot_var_key_lst =
        sorted_map_to_key_list func_map_val.snap_shot_var_map
      in
      let ids =
        List.fold_right
          (fun key acc -> (SVoid, SId key) :: acc)
          snap_shot_var_key_lst []
      in
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
  (* find the parent function *)

  (* need to store the prev state *)
  let old_builder = builder in
  match stmt with
  | SExpr e ->
      (* Printf.printf "terminator %s \n" (str_of_terminator builder); *)
      ignore (build_expr func_map var_map builder e);
      (func_map, var_map, builder)
  | SReturn e ->
      ignore (L.build_ret (build_expr func_map var_map builder e) builder);
      (func_map, var_map, builder)
  | SFunc fdecl ->
      let new_func_map, new_var_map, new_builder =
        build_function func_map var_map fdecl builder
      in
      (* function terminator *)
      add_terminal new_builder
        (L.build_ret (L.const_int (llvm_type fdecl.sret_type) 1));
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
  | SBindAndAssign ((typ, id), (etyp, sx)) ->
      let var =
        match etyp with
        | ST_int shape ->
            let llvm_base_typ = llvm_type SP_int in
            let tensor_typ = llvm_type_of_tensor context llvm_base_typ shape in
            L.build_alloca tensor_typ id builder
        | _ -> L.build_alloca (llvm_type typ) id builder
      in
      let var_val =
        match etyp with
        | ST_int shape -> { typ = etyp; the_var = var }
        | _ -> { typ; the_var = var }
      in
      let new_var_map = StringMap.add id var_val var_map in
      let e' =
        let expr = build_expr func_map var_map builder (etyp, sx) in
        let expr_typ = L.type_of expr in
        match (L.classify_type expr_typ, etyp) with
        | L.TypeKind.Pointer, ST_int _ -> L.build_load expr "elem" builder
        | _ -> expr
      in
      ignore (L.build_store e' var builder);
      (func_map, new_var_map, builder)
  | SIfElse (pred, then_stmt, else_stmt) ->
      let the_function = L.block_parent (L.insertion_block builder) in
      let bool_val = build_expr func_map var_map builder pred in

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

      (* Printf.printf "then_builder is: %s\n" (str_of_terminator then_builder);
         Printf.printf "pred is: %s\n" (string_of_sexpr pred);
         Printf.printf "else_builder is: %s\n" (str_of_terminator else_builder);
         Printf.printf "Cur_end_blcok is: %s\n" (str_of_block end_bb); *)

      (* print_end_block_stack (); *)
      (* push the end bb  *)
      (* push_end_block end_bb;
         print_end_block_stack (); *)
      (* Move the builder to the end block *)
      let end_builder = L.builder_at_end context end_bb in
      (* cur end_builder terminator is the previous terminator  *)
      (func_map, var_map, end_builder)
  | SBlock stmts ->
      let new_func_map, new_var_map, new_builder =
        build_stmt_list func_map var_map builder stmts control_instr_opt
      in
      (* add_terminal builder (L.build_ret (L.const_int (llvm_type A.P_int) 0)); *)
      (match control_instr_opt with
      | Some control_instr -> add_terminal new_builder control_instr
      | None -> ());
      (* (new_func_map, new_var_map, old_builder) *)
      (new_func_map, new_var_map, new_builder)
  | SWhile (pred, body) ->
      let the_function = L.block_parent (L.insertion_block builder) in
      (* check condition  *)
      let while_bb = L.append_block context "while" the_function in
      let build_br_while = L.build_br while_bb in
      (* partial function *)
      ignore (build_br_while builder);
      let while_builder = L.builder_at_end context while_bb in
      let bool_val = build_expr func_map var_map while_builder pred in

      (* the body *)
      let body_bb = L.append_block context "while_body" the_function in
      let body_builder = L.builder_at_end context body_bb in
      let _, _, body_builder_new =
        build_stmt func_map var_map body_builder body (Some build_br_while)
      in
      add_terminal body_builder_new build_br_while;

      (* the end  *)
      let end_bb = L.append_block context "while_end" the_function in

      ignore (L.build_cond_br bool_val body_bb end_bb while_builder);
      let end_builder = L.builder_at_end context end_bb in
      (func_map, var_map, end_builder)
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
        sret_type = SP_int;
        sfname = "main";
        sparams = [];
        sbody = program.sglobals;
      }
  in
  let func_map, global_vars, _ =
    build_stmt func_map global_vars builder main_func None
  in
  the_module
