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
  | Texp_open(_) -> failwith "NI1"

and type_check (_ctx: rty_ctx) (e: Typedtree.expression) (_ty: rty): unit =
  match e.exp_desc with
  | Texp_ident(_)
  | Texp_constant(_) ->
      (match _ty with
      | RtyBase {base_ty; phi} -> 
          failwith "type error"
          (* if base_ty == e.exp_type then
            call z3, convert constant c into predicate v = c, and show forall v, [(v == c) => phi]
          else failwith "type error" *)
      | _ -> failwith "type error")
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
  | Texp_open(_) -> failwith "NI2"

(*
let type_infer_item (_ctx: rty_ctx) (item: Typedtree.structure_item) : rty_exp option =
  match item.str_desc with
  | Tstr_eval (_e, _) -> failwith "NI"
  | Tstr_value (_, _vb) ->
    let (_, ret_ty) = List.find (fun (p, _) -> p == vb.vb_pat) _ctx in 
    type_check _ctx _vb.vb_expr ret_ty ( Regular type checking and add to ctx )
  | _ -> None
*)

let bidirect_type_infer (ctx: rty_ctx) (struc: Typedtree.structure) 
  (ty: rty option) : (* rty_exp_list*) unit =
    List.iter (* filter_map (to make it run first)*)
    (fun (item: Typedtree.structure_item) -> 
      match ty with
      | None -> failwith "NI4" (* type_infer_item ctx item *)
      | Some _ty' -> (* do type check *)
        (match item.str_desc with
        | Tstr_eval (_e, _) -> failwith "NI5"
        | Tstr_value (_, vbs) ->
        (* an ugly workaround to assume that _ctx and vbs have only one element and they match,
        since I don't know how to match a pattern in a parsed tree and a typed tree *)
          (match (ctx, vbs) with
          | ([_, ret_ty], [vb]) -> 
            type_check ctx vb.vb_expr ret_ty
          | _ -> failwith "Expected exactly one context entry and one value binding")
        | _ -> ())
    )
    struc.str_items 
