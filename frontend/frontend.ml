open Ppxlib
open Parsetree
open Ocaml_common
open Rty_lib
open Rty_lib.Rty
open Typecheck

type info = {
  z3_ctx: Z3.context; 
  env: Env.t; 
  cctx: Smtcheck.constr_ctx;
  (*vctx: Smtcheck.val_ctx;*)
  prefix: string;
}

let attr_is_rty attribute = String.equal "rty" attribute.attr_name.txt
let attr_is_axiom attribute = String.equal "axiom" attribute.attr_name.txt
let attr_is_exists attribute = String.equal "exists" attribute.attr_name.txt

let item_is_rty item =
  match item.pstr_desc with
  (* NOTE: omit rec_flag; refinement type cannot be recursive *)
  | Pstr_value (_, [ value_binding ]) ->
    List.exists attr_is_rty value_binding.pvb_attributes
  (* NOTE: omit mutural recursion*)
  | Pstr_value (_, _) -> false
  | _ -> false

let item_is_axiom item =
  match item.pstr_desc with
  | Pstr_value (_, [ value_binding ]) ->
    List.exists attr_is_axiom value_binding.pvb_attributes
  | Pstr_value (_, _) -> false
  | _ -> false

let item_mod_info pt_item =
  let (item, ty_item): 
    Parsetree.structure_item * Typedtree.structure_item 
  = pt_item in
  let pstruc = 
    match item.pstr_desc with
    | Pstr_module {pmb_name = {txt=Some name; _}; pmb_expr = {pmod_desc; _}; _} ->
      (match pmod_desc with
      | Pmod_structure struc -> Some (name, struc)
      | _ -> None)
    | _ -> None
  in
  let tstruc = 
    match ty_item.str_desc with
    | Tstr_module {mb_name = {txt=Some name; _}; mb_expr = {mod_desc; _}; _} ->
      (match mod_desc with
      | Tmod_structure struc -> Some (name, struc)
      | _ -> None)
    | _ -> None
  in
  match pstruc, tstruc with
  | Some (n1, pstruc), Some (n2, tstruc) when n1 = n2 -> 
    Some (n1, pstruc, tstruc)
  | _ -> None

let pat_var_name pat = 
  match pat.ppat_desc with
  | Ppat_var {txt; _} -> txt
  | _ -> failwith "Not variable"

let module_ty_decl items =
  List.filter_map
    (fun item ->
      match item.pstr_desc with
      | Pstr_type(_, [ty]) -> Some(ty.ptype_name.txt)
      | _ -> None)
    items

let module_val items =
  List.filter_map
    (fun item ->
      match item.pstr_desc with
      | Pstr_value(_, [vb]) -> Some(pat_var_name vb.pvb_pat)
      | _ -> None)
    items

let rec module_partition binding =
  let pmod_desc = binding.pmb_expr.pmod_desc in
  match pmod_desc with
  | Pmod_structure struc ->
    let str = remove_attr struc in
    {binding with 
      pmb_expr = 
        {binding.pmb_expr with pmod_desc = Pmod_structure str}}
  | _ -> binding

and remove_attr items =
  List.filter_map
    (fun item ->
      match item.pstr_desc with
      | Pstr_module binding ->
        let binding' = module_partition binding in
        Some ({item with pstr_desc = Pstr_module binding'})
      | _ when item_is_rty item -> None
      | _ when item_is_axiom item -> None
      | _ -> Some item)
    items

let pat_exists pat = 
  List.exists attr_is_exists pat.ppat_attributes

let parse_pat_constr pat = 
  match pat.ppat_desc with
  | Ppat_constraint (pat, ty) -> 
    (pat_var_name pat, ty, pat_exists pat)
  | _ -> failwith "Not constraint"

let pat_var_expr z3_ctx cctx prefix pat ty = 
  match pat.ppat_desc with
  | Ppat_var {txt; _} ->
    let sort = Smtcheck.convert_type z3_ctx cctx prefix ty in
    Z3.Expr.mk_const_s z3_ctx txt sort
  | _ -> failwith "Not variable"

let rec parse_rty info expr =
  match expr.pexp_desc with
  | Pexp_let (_, bindings, ret_rty) ->
    (* Build up the environment *)
    let (new_env, arg_list) =
      List.fold_left
        (fun (env, l) binding ->
          let arg_rty = parse_rty {info with env = env} binding.pvb_expr in
          let ty = rty_to_type_expr arg_rty in
          let val_desc = Ocaml_typecheck.create_val_desc ty in
          let name = pat_var_expr info.z3_ctx info.cctx info.prefix binding.pvb_pat ty in
          let (_, new_env) = Env.enter_value (pat_var_name binding.pvb_pat) val_desc env in
          (new_env, l @ [(name,arg_rty)]))
        (info.env, []) bindings
    in
    List.fold_right
      (fun (arg_name, arg_rty) ret_rty ->
        RtyArrow { arg_name; arg_rty; ret_rty })
      arg_list (parse_rty {info with env = new_env} ret_rty)
  | Pexp_constraint (phi, base_ty) ->
    let base_ty = Ocaml_typecheck.process_type info.env base_ty in
    let val_desc = Ocaml_typecheck.create_val_desc base_ty in
    let (_,env) = Env.enter_value "v" val_desc info.env in
    let phi = 
      Smtcheck.transl_expr 
        info.z3_ctx 
        info.cctx
        []
        (*info.vctx*)
        info.prefix
        (Ocaml_typecheck.process_expr env phi)
    in
    let v = 
      Smtcheck.create_var 
        info.z3_ctx 
        info.cctx 
        info.prefix 
        ("var_" ^ info.prefix ^ "v") 
        base_ty 
    in
    let x =
      Smtcheck.create_var 
        info.z3_ctx 
        info.cctx 
        info.prefix 
        "v" 
        base_ty 
    in
    let phi = Z3.Expr.substitute_one phi v x in
    RtyBase { base_ty ; phi }
  | _ -> failwith "nope rty"

let rec parse_axiom info arg_sorts arg_syms expr = 
  match expr.pexp_desc with
  | Pexp_constraint(phi, base_ty) ->
    let base_ty = Ocaml_typecheck.process_type info.env base_ty in
    let phi_typed = Ocaml_typecheck.process_expr info.env phi in
    if Ocaml_helper.unify_base_type info.env base_ty Predef.type_bool 
      && Ocaml_helper.unify_base_type info.env phi_typed.exp_type Predef.type_bool then
      let bound_vars = List.map Z3.Symbol.to_string arg_syms in
      let constr = 
        Smtcheck.transl_expr 
          info.z3_ctx
          info.cctx
          bound_vars
          (*info.vctx*)
          info.prefix
          phi_typed
      in
      let fin_expr = Z3.Boolean.mk_eq info.z3_ctx (constr) (Z3.Boolean.mk_true info.z3_ctx) in
      Z3.Quantifier.expr_of_quantifier 
        (Z3.Quantifier.mk_forall 
          info.z3_ctx arg_sorts arg_syms fin_expr None [] [] None None)
    else
      failwith "axiom return type is not bool"
  | Pexp_fun (_, _, arg, body) ->
    let (name, tyc, _) = parse_pat_constr arg in
    let ty = Ocaml_typecheck.process_type info.env tyc in
    let val_desc = Ocaml_typecheck.create_val_desc ty in
    let (_, new_env) = Env.enter_value name val_desc info.env in
    let sym_name = "var_" ^ info.prefix ^ name in
    let sym = Z3.Symbol.mk_string info.z3_ctx sym_name in
    let sort = Smtcheck.convert_type info.z3_ctx info.cctx info.prefix ty in
    parse_axiom {info with env = new_env} (arg_sorts @ [sort]) (arg_syms @ [sym]) body 
    (*let ret = 
      if exists then
        Z3.Quantifier.mk_exists info.z3_ctx [sort] [sym] phi None [] [] None None
      else
        Z3.Quantifier.mk_forall info.z3_ctx [sort] [sym] phi None [] [] None None 
    in
    Z3.Quantifier.expr_of_quantifier ret*)
  | _ -> 
    let phi_typed = Ocaml_typecheck.process_expr info.env expr in
    let bound_vars = List.map Z3.Symbol.to_string arg_syms in
    let constr = 
      Smtcheck.transl_expr 
        info.z3_ctx 
        info.cctx
        bound_vars
        (*info.vctx*)
        info.prefix
        phi_typed
    in
    let fin_expr = Z3.Boolean.mk_eq info.z3_ctx (constr) (Z3.Boolean.mk_true info.z3_ctx) in
    Z3.Quantifier.expr_of_quantifier 
      (Z3.Quantifier.mk_forall 
        info.z3_ctx arg_sorts arg_syms fin_expr None [] [] None None)
    

let parse_rty_binding info value_binding =
  (info.prefix ^ (pat_var_name value_binding.pvb_pat), info.prefix,
    parse_rty info value_binding.pvb_expr)

let parse_axiom_binding info value_binding =
  (*print_endline (Ocaml_helper.string_of_pattern value_binding.pvb_pat);
  print_endline (Ocaml_helper.string_of_expression value_binding.pvb_expr);*)
  let phi = parse_axiom info [] [] value_binding.pvb_expr in
  print_endline (Z3.Expr.to_string phi);
  ("", info.prefix, RtyBase{ base_ty = Predef.type_int; phi})

let get_impl_from_typed_items name prefix struc =
  let open Ocaml_common.Typedtree in
  List.find_map
    (fun str ->
      match str.str_desc with
      | Tstr_value (_, [ value_binding ]) ->
          let pat =
            Ocaml_common.Untypeast.untype_pattern value_binding.vb_pat
          in
          if String.equal (prefix ^ Ocaml_helper.string_of_pattern pat) name then
            Some value_binding.vb_expr
          else None
      | _ -> None)
    struc.str_items

let rec type_struc 
  (z3_ctx: Z3.context)
  (top_ctx: rty_ctx)
  (top_cctx: Smtcheck.constr_ctx)
  (path: string list)
  (struc: Parsetree.structure) 
  (ty_struc: Typedtree.structure): (rty_ctx * Smtcheck.constr_ctx) =
  let rtys, ax_struc = List.partition item_is_rty struc in
  let axioms, struc = List.partition item_is_axiom ax_struc in
  
  let ty_items = ty_struc.str_items in
  let pt_struc = List.map2 (fun x y -> (x,y)) struc ty_items in
  let mod_info = List.filter_map item_mod_info pt_struc in

  let prefix = List.fold_left (fun acc x -> acc ^ x ^ ".") "" path in
  let ty_decl_names = module_ty_decl struc in
  (*let val_names = module_val struc in*)
  let cctx: Smtcheck.constr_ctx = 
    List.map 
      (fun name -> 
        let sort = Z3.Sort.mk_uninterpreted_s z3_ctx (prefix ^ name) in
        (prefix ^ name, sort)) 
      ty_decl_names 
  in
  (*let vctx: Smtcheck.val_ctx = List.map (fun name -> (name, prefix ^ name)) val_names in*)

  let env = ty_struc.str_final_env in
  let info = {z3_ctx; env; cctx; (*vctx;*) prefix} in
  let rtys_ctx: rty_ctx =
    List.filter_map
      (fun item ->
        match item.pstr_desc with
        | Pstr_value (_, [ value_binding ]) ->
          Some (parse_rty_binding info value_binding)
        | _ -> None)
      rtys
  in
  let axioms_ctx: rty_ctx =
    List.filter_map
      (fun item ->
        match item.pstr_desc with
        | Pstr_value(_, [ value_binding ]) ->
          Some (parse_axiom_binding info value_binding)
        | _ -> None)
      axioms
  in
  (* There may be issue with names inside modules *)
  (* Also order of the type declaration 
    and implementation is not checked*)
  let top_ctx = axioms_ctx @ rtys_ctx @ top_ctx in
  let top_cctx = cctx @ top_cctx in
  let (module_rctx, module_cctx) = 
    List.fold_left 
      (fun (rctx, cctx) (name, pstruc, tstruc) -> 
        type_struc z3_ctx rctx cctx (path @ [name]) pstruc tstruc) 
      (top_ctx, top_cctx) 
      mod_info 
  in
  let anf_struc = Anormal.normalize ty_struc in
  let ret_ctx = Rtycheck.bidirect_type z3_ctx module_rctx module_cctx (*vctx*) prefix anf_struc in
  
  let () =
    List.iter
      (fun (name, _, rty) ->
        match get_impl_from_typed_items name prefix ty_struc with
        | None ->
            Printf.printf "cannot find the implementation of function %s\n"
              (name)
        | Some impl ->
            Printf.printf "Type judgement [%s]\n|-\n%s\n: %s\n"
              (name)
              (Pprintast.string_of_expression
              @@ Ocaml_common.Untypeast.untype_expression impl)
              (Rty.layout_rty rty))
      rtys_ctx
  in
  (ret_ctx.rty, cctx)

let impl struc =
  let ret_struc = remove_attr struc in
  let implementation = Ocaml_typecheck.process_implementation_file ret_struc in
  let ty_struc = implementation.structure in
  
  let z3_ctx = Z3.mk_context [] in
  let builtin_ctx = Rty.Builtin.add_builtins z3_ctx [] in
  let _ = type_struc z3_ctx builtin_ctx [] [] struc ty_struc in
  ret_struc

let intf intf = intf
let () = Driver.register_transformation ~impl ~intf "refinement type"
