open Ppxlib
open Parsetree
open Ocaml_common
open Rty
open Typecheck
(* open Z3 *)
(* open Smtcheck *)

let string_of_pattern pattern =
  let _ = Format.flush_str_formatter () in
  Pprintast.pattern Format.str_formatter pattern;
  Format.flush_str_formatter ()

let string_of_type_expr ty =
  let _ = Format.flush_str_formatter () in
  Format.fprintf Format.str_formatter "%a"
  Printtyp.type_expr ty;
  Format.flush_str_formatter ()

let rec layout_rty = function
  | RtyBase { base_ty; phi } ->
      (* let phi = 
        Ocaml_common.Untypeast.untype_expression phi
      in *)
      Printf.sprintf "{v:%s | %s}"
        (string_of_type_expr base_ty)
        (* (Pprintast.string_of_expression phi) *)
        (Z3.Expr.to_string phi)
  | RtyArrow { arg_name; arg_rty; ret_rty } ->
      Printf.sprintf "%s:%s -> %s"
        arg_name
        (layout_rty arg_rty) (layout_rty ret_rty)

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
  | RtyArrow { arg_name ; arg_rty; ret_rty } ->
      let arg_ty = rty_to_type_expr arg_rty in
      let ret_ty = rty_to_type_expr ret_rty in
      Types.create_expr 
        (Tarrow (Labelled arg_name, arg_ty, ret_ty, Types.commu_ok))
        ~level:0 ~scope:0 ~id:0

let rec parse_rty env expr =
  match expr.pexp_desc with
  | Pexp_let (_, bindings, ret_rty) ->
      (* Build up the environment *)
      let (new_env, arg_list) =
        List.fold_left
          (fun (env, l) binding ->
            let arg_rty = parse_rty env binding.pvb_expr in
            let ty = rty_to_type_expr arg_rty in
            let val_desc = Ocaml_typecheck.create_val_desc ty in
            match binding.pvb_pat.ppat_desc with
            | Ppat_var {txt; _} -> 
                let (_, new_env) = Env.enter_value txt val_desc env in
                (new_env, l @ [(txt,arg_rty)])
            | _ -> failwith "not a variable")
          (env, []) bindings
      in
      List.fold_right
        (fun (arg_name, arg_rty) ret_rty ->
          RtyArrow { arg_name; arg_rty; ret_rty })
        arg_list (parse_rty new_env ret_rty)
  | Pexp_constraint (phi, base_ty) ->
      let base_ty = Ocaml_typecheck.process_type env base_ty in
      let val_desc = Ocaml_typecheck.create_val_desc base_ty in
      let (_,env) = Env.enter_value "v" val_desc env in
      let phi = Ocaml_typecheck.process_expr env phi in
      (* Convert typetree to z3 expression*)
      let phi_z3 = Smtcheck.convert_phi !Contexts.z3_ctx phi in 
      RtyBase { base_ty = base_ty ; phi = phi_z3 }
  | _ -> failwith "die"

let parse_rty_binding value_binding =
  let name = 
    match value_binding.pvb_pat.ppat_desc with
    | Ppat_var {txt; _} -> txt
    | _ -> failwith "Not variable"
  in
  (name, 
   parse_rty 
    (Ocaml_typecheck.initial_env ()) 
    value_binding.pvb_expr)

let get_impl_from_typed_items name implementation =
  let open Ocaml_common.Typedtree in
  List.find_map
    (fun str ->
      match str.str_desc with
      | Tstr_value (_, [ value_binding ]) ->
          let pat =
            Ocaml_common.Untypeast.untype_pattern value_binding.vb_pat
          in
          if String.equal (string_of_pattern pat) name then
            Some value_binding.vb_expr
          else None
      | _ -> None)
    implementation.structure.str_items

let impl struc =
  let rtys, struc = List.partition item_is_rty struc in
  let implementation = Ocaml_typecheck.process_implementation_file struc in
  let rtys_ctx: rty_ctx =
    List.filter_map
      (fun item ->
        match item.pstr_desc with
        | Pstr_value (_, [ value_binding ]) ->
            Some (parse_rty_binding value_binding)
        | _ -> None)
      rtys
  in
  (* assign rty context in Contexts *)
  Contexts.rtys_ctx := rtys_ctx;
  let _ = Rtycheck.bidirect_type_infer (* rtys_ctx *) implementation.structure None in
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
              (layout_rty rty))
      rtys_ctx
  in
  struc

let intf intf = intf
let () = Driver.register_transformation ~impl ~intf "refinement type"
