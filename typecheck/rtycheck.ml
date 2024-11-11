open Ocaml_common
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

let rec subst (ty: rty) (name: Expr.expr) (expr: Expr.expr): rty =
  match ty with
  | RtyBase {base_ty; phi} ->
    let phi = Expr.substitute_one phi name expr in
    RtyBase {base_ty; phi}
  | RtyArrow {arg_name; arg_rty; ret_rty} ->
    let arg_rty = subst arg_rty name expr in 
    if Expr.equal arg_name name then 
      RtyArrow {arg_name; arg_rty; ret_rty}
    else
      let ret_rty = subst arg_rty name expr in
      RtyArrow {arg_name; arg_rty; ret_rty}

(* use arg_name to replace it with v *)
let entailment (ctx: full_ctx) (pred: Expr.expr): Expr.expr =
  List.fold_left
    (fun pred (name, ty) ->
      match ty with
      | RtyBase {base_ty; phi} ->
        let v = Smtcheck.create_var ctx.z3 "v" base_ty in
        let x = Smtcheck.create_var ctx.z3 name base_ty in
        let phi = Expr.substitute_one phi v x in
        Boolean.mk_implies ctx.z3 phi pred
      | RtyArrow (_) -> pred)
    pred ctx.rty

let check_subtype (env: Env.t) (ctx: full_ctx) (ty': rty) (ty: rty) =
  match ty', ty with
  | RtyBase{base_ty = bty1; phi = phi1}, RtyBase{base_ty = bty2; phi = phi2} 
    when unify_base_type env bty1 bty2 ->
      let c = entailment ctx (Boolean.mk_implies ctx.z3 phi1 phi2) in
      Smtcheck.check ctx.z3 c
  | _ -> failwith "NI CHECK_SUBTYPE"

let rec type_infer (ctx: full_ctx) (e: Typedtree.expression) : rty =
  match e.exp_desc with
  | Texp_ident (path, _, _) ->
    let name = Path.name path in
    (match ctx_lookup ctx.rty name with
    | None -> failwith "Need to implement function that converts type_expr to Rty"
    | Some (_, ty) -> ty)
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
  | Texp_apply(op, args) ->
    (* Note: what if op does not have a refinement type? See Texp_ident*)
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
  | Texp_ident (path, _, value_desc) ->
      let name = Path.name path in
      let ty' = 
        (match ctx_lookup ctx.rty name with
        | None -> RtyBase{base_ty=value_desc.val_type; phi=Boolean.mk_true ctx.z3}
        | Some (_, ty') -> ty')
      in
      (* Check if this is correct *)
      check_subtype e.exp_env ctx ty' ty
  | Texp_apply(_)
  | Texp_constant(_) ->
    let ty' = type_infer ctx e in
    check_subtype e.exp_env ctx ty' ty
  | Texp_function {param; cases = [{c_rhs; _}]; _} ->
    (match ty with
    | RtyBase (_) -> failwith "Type error: Function being analyzed with RtyBase type"
    | RtyArrow {arg_name; arg_rty; ret_rty;} ->
      
      (* check that arg_name is a Z3 variable which has the same name as name *)
      if (Z3.Expr.to_string arg_name) <> (Ident.name param) then
        failwith (Printf.sprintf "name mismatch for parameter %s and argument %s\n" 
          (Ident.name param) (Z3.Expr.to_string arg_name));
      
      let new_ctx = {z3 = ctx.z3; rty = (Ident.name param, arg_rty)::ctx.rty} in
      type_check new_ctx c_rhs ret_rty)
  | Texp_let(Nonrecursive, [vb], expr) ->
    let rty1 = type_infer ctx vb.vb_expr in
    (match vb.vb_pat.pat_desc with
    | Tpat_var(ident, _) ->
      let name = Ident.name ident in
      let new_ctx = {z3 = ctx.z3; rty = (name, rty1)::ctx.rty} in
      type_check new_ctx expr ty
    | _ -> failwith "other cases in let pat")
  | Texp_function(_)
  | Texp_let(_)
  | Texp_match(_)
  | Texp_try(_)
  | Texp_tuple(_) -> failwith "NI TYPE_CHECK_3"
  | Texp_construct(_, {cstr_name; _}, _) ->
    (match cstr_name with
    | ("true" | "false") ->
      let ty' = type_infer ctx e in
      check_subtype e.exp_env ctx ty' ty
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
  | Tstr_eval (e, _) ->  Some (e, type_infer ctx e) 
  | Tstr_value (_, [vb]) ->
    (* Fix: get ctx.rty up to pat *)
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
