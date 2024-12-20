open Ppxlib
open Parsetree
open Ocaml_common
open Rty_lib
open Rty_lib.Rty
open Typecheck
open Anormal

let attr_is_rty attribute = String.equal "rty" attribute.attr_name.txt

let item_is_rty item =
  match item.pstr_desc with
  (* NOTE: omit rec_flag; refinement type cannot be recursive *)
  | Pstr_value (_, [ value_binding ]) ->
      List.exists attr_is_rty value_binding.pvb_attributes
  (* NOTE: omit mutural recursion*)
  | Pstr_value (_, _) -> false
  | _ -> false

let rec rty_to_type_expr (rty: rty): Types.type_expr =
  match rty with
  | RtyBase { base_ty; _ } -> base_ty
  | RtyArrow { arg_rty; ret_rty; _ } ->
      let arg_ty = rty_to_type_expr arg_rty in
      let ret_ty = rty_to_type_expr ret_rty in
      Types.create_expr 
        (Tarrow (Nolabel, arg_ty, ret_ty, Types.commu_ok))
        ~level:0 ~scope:0 ~id:0

let pat_var_name pat = 
  match pat.ppat_desc with
  | Ppat_var {txt; _} -> txt
  | _ -> failwith "Not variable"

let pat_var_expr z3_ctx pat ty = 
  match pat.ppat_desc with
  | Ppat_var {txt; _} ->
    let sort = Smtcheck.convert_type z3_ctx ty in
    Z3.Expr.mk_const_s z3_ctx txt sort
  | _ -> failwith "Not variable"

let rec parse_rty z3_ctx env expr =
  match expr.pexp_desc with
  | Pexp_let (_, bindings, ret_rty) ->
      (* Build up the environment *)
      let (new_env, arg_list) =
        List.fold_left
          (fun (env, l) binding ->
            let arg_rty = parse_rty z3_ctx env binding.pvb_expr in
            let ty = rty_to_type_expr arg_rty in
            let val_desc = Ocaml_typecheck.create_val_desc ty in
            let name = pat_var_expr z3_ctx binding.pvb_pat ty in
            let (_, new_env) = Env.enter_value (pat_var_name binding.pvb_pat) val_desc env in
            (new_env, l @ [(name,arg_rty)]))
          (env, []) bindings
      in
      List.fold_right
        (fun (arg_name, arg_rty) ret_rty ->
          RtyArrow { arg_name; arg_rty; ret_rty })
        arg_list (parse_rty z3_ctx new_env ret_rty)
  | Pexp_constraint (phi, base_ty) ->
      let base_ty = Ocaml_typecheck.process_type env base_ty in
      let val_desc = Ocaml_typecheck.create_val_desc base_ty in
      let (_,env) = Env.enter_value "v" val_desc env in
      let phi = 
        Smtcheck.convert_phi z3_ctx (Ocaml_typecheck.process_expr env phi)
      in
      let v = Smtcheck.create_var z3_ctx "var_v" base_ty in
      let x = Smtcheck.create_var z3_ctx "v" base_ty in
      let phi = Z3.Expr.substitute_one phi v x in
      RtyBase { base_ty ; phi }
  | _ -> failwith "die"

let parse_rty_binding z3_ctx env value_binding =
  (pat_var_name value_binding.pvb_pat, 
    parse_rty z3_ctx env value_binding.pvb_expr)

let get_impl_from_typed_items name implementation =
  let open Ocaml_common.Typedtree in
  List.find_map
    (fun str ->
      match str.str_desc with
      | Tstr_value (_, [ value_binding ]) ->
          let pat =
            Ocaml_common.Untypeast.untype_pattern value_binding.vb_pat
          in
          if String.equal (Ocaml_helper.string_of_pattern pat) name then
            Some value_binding.vb_expr
          else None
      | _ -> None)
    implementation.structure.str_items

let impl struc =
  let rtys, struc = List.partition item_is_rty struc in
  let implementation = Ocaml_typecheck.process_implementation_file struc in

  let z3_ctx = Z3.mk_context [] in
  let anormal_struc = normalize implementation.structure in
  let env = anormal_struc.str_final_env in
  let rtys_ctx: rty_ctx =
    List.filter_map
      (fun item ->
        match item.pstr_desc with
        | Pstr_value (_, [ value_binding ]) ->
            Some (parse_rty_binding z3_ctx env value_binding)
        | _ -> None)
      rtys
  in
  let rtys_ctx = Rty.Builtin.add_builtins z3_ctx rtys_ctx in
  let _ = Rtycheck.bidirect_type_infer z3_ctx rtys_ctx anormal_struc None in
  let () =
    List.iter
      (fun (name, rty) ->
        match get_impl_from_typed_items name implementation with
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
  struc

let intf intf = intf
let () = Driver.register_transformation ~impl ~intf "refinement type"
