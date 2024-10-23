open Ocaml_common
open Rty_lib
open Rty_lib.Rty
open Z3

type full_ctx = { z3: Z3.context; rty: rty_ctx }

let ctx_lookup (ctx: rty_ctx) (ident: string): (string * rty) option =
  List.find_opt (fun (name, _) -> String.equal name ident) ctx 

let ctx_pat_lookup (ctx: rty_ctx) (pat: Typedtree.pattern): (string * rty) option =
  match pat.pat_desc with
  | Tpat_var(_, {txt=pat_name; _}) -> ctx_lookup ctx pat_name
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

let rec subst (ty: rty) (name: Expr.expr) (expr: Expr.expr): rty =
  match ty with
  | RtyBase {base_ty; phi} ->
    let phi = Expr.substitute_one phi name expr in
    RtyBase {base_ty; phi}
  | RtyArrow {arg_name; arg_rty; ret_rty} ->
    let arg_rty = subst arg_rty name expr in 
    if Expr.equal arg_name name 
    then 
      RtyArrow {arg_name; arg_rty; ret_rty}
    else
      let ret_rty = subst arg_rty name expr in
      RtyArrow {arg_name; arg_rty; ret_rty}

let rec type_infer (ctx: full_ctx) (e: Typedtree.expression) : rty =
  match e.exp_desc with
  | Texp_ident (_, {txt=Longident.Lident name; _}, _) ->
    (match ctx_lookup ctx.rty name with
    | None -> failwith "Need to implement function that converts type_expr to Rty"
    | Some (_, ty) -> ty)
  | Texp_ident (_) -> failwith "Other ident not supported"
  | Texp_constant(value) ->
      let sort = Smtcheck.convert_type ctx.z3 e.exp_type in
      RtyBase
        {
          base_ty = e.exp_type;
          phi = Boolean.mk_eq ctx.z3 
            (Expr.mk_const_s ctx.z3 "v" sort) 
            (Smtcheck.convert_constant ctx.z3 value)
        }
  | Texp_let(_) -> failwith "NI TYPE_INFER for Texp_let"
  | Texp_function(_) -> failwith "Temporary" 
    (*(let param_name = Ident.name param in
    let param_type_expr =
      (match Types.get_desc e.exp_type with
        | Types.Tarrow (_, param_type_expr, _, _) ->
                       (param_type_expr)
        | _ -> failwith "Expected a function type") in
    (* get arg_rty *)
    let argument_rty = RtyBase {
      base_ty = param_type_expr;
      phi = Boolean.mk_eq ctx.z3 
        (Arithmetic.Integer.mk_const_s ctx.z3 "v") 
        (Arithmetic.Integer.mk_const_s ctx.z3 param_name)
    } in
    (match cases with
      | [case] ->
        let body = case.c_rhs in
        (* get ret_rty recursively *)
        let return_rty = type_infer ctx body in
        RtyArrow {arg_name = param_name; arg_rty = argument_rty; ret_rty = return_rty}
      | _ -> failwith "Unexpected number of cases in function"))*)
  | Texp_apply(op, args) ->
    let ty = type_infer ctx op in
    let arg_exprs = 
      List.map 
        (fun arg -> 
          match arg with
          | (_, Some e) -> e 
          | _ -> failwith "Labelled partial ap not supported")
        args
    in
    let (arg_names, final_ty) = 
      List.fold_left 
        (fun (l, arg_rty) arg ->
          match arg_rty with
          | RtyArrow {arg_name; arg_rty; ret_rty} ->
            type_check ctx arg arg_rty; (l @ [arg_name], ret_rty)
          | _ -> failwith "Not rty arrow")
        ([], ty) arg_exprs
    in
    let arg_z3_exprs = 
      List.map (fun arg -> Smtcheck.transl_expr ctx.z3 arg) arg_exprs
    in
    List.fold_left2
      (fun ty arg_name arg_expr -> subst ty arg_name arg_expr)
      final_ty arg_names arg_z3_exprs
  | Texp_match(_)
  | Texp_try(_)
  | Texp_tuple(_)
  | Texp_construct(_) ->
      (* only works for booleans *)
      let sort = Smtcheck.convert_type ctx.z3 e.exp_type in
      RtyBase
        {
          base_ty = e.exp_type;
          phi = Boolean.mk_eq ctx.z3 
            (Expr.mk_const_s ctx.z3 "v" sort) 
            (Smtcheck.transl_expr ctx.z3 e)
        }
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
  | Texp_apply(_)
  | Texp_constant(_) ->
    let ty' = type_infer ctx e in
    check_subtype e.exp_env ctx.z3 ty' ty
  | Texp_function (_) ->
    (* infer the type of parameter and return value of the functions. 
    todo: Here, param contains x and the body of cases contains x + 2.
    We need to unprove x in int => x + 2 > 3. 
    First, we infer that rty is {int, v = x} {int, v' = x + 2};
    Then, we prove {v = x and true} => {v' = x + 2, v' > 3} *)
    let ty' = type_infer ctx e in
    Printf.printf "%s" (Rty.layout_rty ty');
    check_subtype e.exp_env ctx.z3 ty' ty
  | Texp_let(_)
  | Texp_match(_)
  | Texp_try(_)
  | Texp_tuple(_) -> failwith "NI TYPE_CHECK_3"
  | Texp_construct(_, {cstr_name; _}, _) ->
    (match cstr_name with
    | ("true" | "false") ->
      let ty' = type_infer ctx e in
      check_subtype e.exp_env ctx.z3 ty' ty
    | _ -> failwith "Other constructors not supported")
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
    (let pty = ctx_pat_lookup ctx.rty vb.vb_pat in
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
