open Ocaml_common
open Rty

let type_infer (_ctx: rty_ctx) (e: Typedtree.expression) : rty_exp =
  (* Some of these will not be implemented *)
  match e.exp_desc with
  | Texp_ident(_)
  | Texp_constant(_)
  | Texp_let(_)
  | Texp_function(_)
  | Texp_apply(_)
  | Texp_match(_)
  | Texp_try(_)
  | Texp_tuple(_)
  | Texp_construct(_)
  | Texp_variant(_)
  | Texp_record(_)
  | Texp_field(_)
  | Texp_setfield(_)
  | Texp_array(_)
  | Texp_ifthenelse(_)
  | Texp_sequence(_)
  | Texp_while(_)
  | Texp_for(_)
  | Texp_send(_)
  | Texp_new(_)
  | Texp_instvar(_)
  | Texp_setinstvar(_)
  | Texp_override(_)
  | Texp_letmodule(_)
  | Texp_letexception(_)
  | Texp_assert(_)
  | Texp_lazy(_)
  | Texp_object(_)
  | Texp_pack(_)
  | Texp_letop(_)
  | Texp_unreachable
  | Texp_extension_constructor(_)
  | Texp_open(_) -> failwith "NI"

and type_check (_ctx: rty_ctx) (e: Typedtree.expression) (_ty: rty): rty_exp =
  match e.exp_desc with
  | Texp_ident(_)
  | Texp_constant(_)
  | Texp_let(_)
  | Texp_function(_)
  | Texp_apply(_)
  | Texp_match(_)
  | Texp_try(_)
  | Texp_tuple(_)
  | Texp_construct(_)
  | Texp_variant(_)
  | Texp_record(_)
  | Texp_field(_)
  | Texp_setfield(_)
  | Texp_array(_)
  | Texp_ifthenelse(_)
  | Texp_sequence(_)
  | Texp_while(_)
  | Texp_for(_)
  | Texp_send(_)
  | Texp_new(_)
  | Texp_instvar(_)
  | Texp_setinstvar(_)
  | Texp_override(_)
  | Texp_letmodule(_)
  | Texp_letexception(_)
  | Texp_assert(_)
  | Texp_lazy(_)
  | Texp_object(_)
  | Texp_pack(_)
  | Texp_letop(_)
  | Texp_unreachable
  | Texp_extension_constructor(_)
  | Texp_open(_) -> failwith "NI"

let type_infer_item (_ctx: rty_ctx) (item: Typedtree.structure_item) : rty_exp option =
  match item.str_desc with
  | Tstr_eval (_e, _) -> failwith "NI"
  | Tstr_value (_, _vb) -> failwith "NI" (* Regular type checking and add to ctx*)
  | _ -> None

let bidirect_type_infer (ctx: rty_ctx) (struc: Typedtree.structure) 
  (ty: rty option) : rty_exp_list =
    List.filter_map
    (fun item -> 
      match ty with
      | None -> type_infer_item ctx item 
      | Some _ty' -> failwith "NI" (* Then do type check *))
    struc.str_items 
