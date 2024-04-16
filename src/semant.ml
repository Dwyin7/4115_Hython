open Ast
open Sast
module StringMap = Map.Make (String)

(* semantic checking of ast, return Sast if success *)
(* globals are list of statements  *)
(* function and variable share the same name space under same scope *)
let check (imports, globals) =
  (* TODO: Verify a list of bindings has no duplicate names  *)
  (* cannot have same var name in same scope *)


  (* TODO: check expr *)

  (* TODO: check tensor *)

  (* TODO: check lambda *)

  (* TODO: check Call *)



  (* TODO: Add Function name to symbol table *)

  (* TODO: Find Function name in symbol table *)






  (* TODO: check stmt *)

  (* TODO: check Block *)

  (* TODO: check Assign *)

  (* TODO: check Binds *)

  (* TODO: check BindAndAssign *)

  (* TODO: check Func *)

  (* TODO: check function formals *)

  


  (* TODO: check If *)

  (* TODO: check While *)

  (* TODO: check For *)





  let check_binds (kind : string) (binds : (typ * id) list) =
    let rec dups = function
      | [] -> ()
      | (_, n1) :: (_, n2) :: _ when n1 = n2 ->
          raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in
    dups (List.sort (fun (_, a) (_, b) -> compare a b) binds)
  in

  let add_func map (fd : typ * id * bind list * stmt list) =
    let return_type, fname, bindings, statements = fd in
    let dup_error = "duplicate function " ^ fname
    and make_err er = raise (Failure er) in
    match fname with
    | _ when StringMap.mem fname map -> make_err dup_error
    | _ -> StringMap.add fname fd map
  in

  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in




  let check_statment_list (stmts : stmt list) = stmts in

  let check_statment x = x in
  (* function
      | Block -> SBlock ()
      | Assign -> SAssign ()
      |
     in *)
  let check_globals (globals : stmt list) = globals in
  (imports, List.map check_globals globals)
