open Ocaml_common
open Rty
open Z3

type full_ctx = { z3: Z3.context; rty: rty_ctx }

let ctx_lookup (ctx: rty_ctx) (pat: Typedtree.pattern): (string * rty) option =
    match pat.pat_desc with
    | Tpat_var(_, {txt=pat_name; _}) ->
      List.find_opt
        (fun (name, _) -> String.equal name pat_name)
        ctx
    | _ -> None

let unify_base_type (env: Env.t) (ty: Types.type_expr) (ty': Types.type_expr): bool =
  try 
    Ctype.unify env ty ty';
    true
  with
  | _ -> false

let check_subtype (env: Env.t) (ctx: Z3.context) (ty': rty) (ty: rty)=
  match ty', ty with
  | RtyBase{base_ty = bty1; phi = phi1}, RtyBase{base_ty = bty2; phi = phi2} 
    when unify_base_type env bty1 bty2 ->  
    Smtcheck.check ctx phi1 phi2
  | _ -> failwith "NI CHECK_SUBTYPE"

let rec type_infer (ctx: full_ctx) (e: Typedtree.expression) : rty =
  match e.exp_desc with
  | Texp_ident(_) -> failwith "NI TYPE_INFER"
  | Texp_constant(value) ->
      RtyBase
        {
          base_ty = e.exp_type;
          phi = Boolean.mk_eq ctx.z3 
            (Arithmetic.Integer.mk_const_s ctx.z3 "v") 
            (Smtcheck.convert_constant ctx.z3 value)
        }
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
  | Texp_open(_) -> failwith "NI TYPE_INFER_2"

and type_check (ctx: full_ctx) (e: Typedtree.expression) (ty: rty): unit =
  match e.exp_desc with
  | Texp_ident(_) -> failwith "NI TYPE_CHECK"
  | Texp_constant(_) -> (
    let ty' = type_infer ctx e in
    check_subtype e.exp_env ctx.z3 ty' ty
     (* let ty = type_infer ctx e in
    (match ty with
    | RtyBase {base_ty=bty; phi=phi} 
        when unify_base_type bty e.exp_type ->
        Printf.printf "Call z3 here"
    | _ -> failwith "Error at parsing refinement type at constant")*) 
  )
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
  | Texp_open(_) -> failwith "NI TYPE_CHECK 2"

let type_infer_item (ctx: full_ctx) (item: Typedtree.structure_item) : rty_exp option =
  match item.str_desc with
  | Tstr_eval (_e, _) -> None
  | Tstr_value (_, [vb]) ->
    (let pty = ctx_lookup ctx.rty vb.vb_pat in
    match pty with
    | None -> None
    | Some (_, ty) -> type_check ctx vb.vb_expr ty; None)
  | _ -> None

let bidirect_type_infer 
  (z3_ctx: Z3.context) 
  (rctx: rty_ctx) 
  (struc: Typedtree.structure) 
  (_ty: rty option) : rty_exp_list =
    List.filter_map
    (fun item -> type_infer_item { z3=z3_ctx; rty=rctx } item)
    struc.str_items 
