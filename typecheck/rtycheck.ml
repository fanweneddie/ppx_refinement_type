open Ocaml_common
open Rty_lib.Rty
open Rty_lib.Ocaml_helper
open Z3

type full_ctx = {
  z3: Z3.context; 
  rty: rty_ctx; 
  curr: rty_ctx; 
  ctr: Smtcheck.constr_ctx;
}

type ret_ctx = {z3: Z3.context; rty: rty_ctx}

let ctx_lookup (ctx: rty_ctx) (ident: string): (string * rty) option =
  List.find_opt (fun (name, _) -> String.equal name ident) ctx

let get_pat_str (pat: Typedtree.pattern): string =
  match pat.pat_desc with
  | Tpat_var(ident, _) -> Ident.name ident
  | _ -> "--"

let ctx_pat_lookup 
  (ctx: rty_ctx) 
  (prefix: string) 
  (pat: Typedtree.pattern): 
    (string * rty) option =
  ctx_lookup ctx @@ (prefix ^ get_pat_str pat)

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
        let v = Smtcheck.create_var ctx.z3 ctx.ctr "v" base_ty in
        let x = Smtcheck.create_var ctx.z3 ctx.ctr ("var_" ^ name) base_ty in
        let phi = Expr.substitute_one phi v x in
        Boolean.mk_implies ctx.z3 phi pred
      | RtyArrow (_) -> pred)
    pred ctx.rty

let check_subtype (env: Env.t) (ctx: full_ctx) (ty': string * rty) (ty: string * rty) =
  match ty', ty with
  | (name1, RtyBase{base_ty = bty1; phi = phi1}), (name2, RtyBase{base_ty = bty2; phi = phi2})
    when unify_base_type env bty1 bty2 ->
      let v = Smtcheck.create_var ctx.z3 ctx.ctr name2 bty1 in
      let x = Smtcheck.create_var ctx.z3 ctx.ctr name1 bty2 in
      let phi2 = Expr.substitute_one phi2 v x in
      let c = entailment ctx (Boolean.mk_implies ctx.z3 phi1 phi2) in
      Smtcheck.check ctx.z3 c
  | _ -> failwith "NI CHECK_SUBTYPE"

let rec type_infer (ctx: full_ctx) (e: Typedtree.expression) : rty =
  match e.exp_desc with
  | Texp_ident (path, _, value_desc) ->
    let name = Path.name path in
    (match ctx_lookup (ctx.rty @ ctx.curr) name with
    | None -> 
      RtyBase { base_ty = value_desc.val_type; phi = Boolean.mk_true ctx.z3 }
    | Some (_, ty) -> ty)
  | Texp_constant(value) ->
      let sort = Smtcheck.convert_type ctx.z3 ctx.ctr e.exp_type in
      RtyBase
        {
          base_ty = e.exp_type;
          phi = Boolean.mk_eq ctx.z3 
            (Expr.mk_const_s ctx.z3 "v" sort)
            (Smtcheck.convert_constant ctx.z3 value)
        }
  | Texp_let(_, [vb], expr) -> 
    let rty1 = type_infer ctx vb.vb_expr in
    (match vb.vb_pat.pat_desc with
    | Tpat_var(ident, _) ->
      let name = Ident.name ident in
      let new_ctx = {ctx with rty = (name, rty1)::ctx.rty} in
      type_infer new_ctx expr
    | _ -> failwith "type_infer: other cases in let pat")
  | Texp_let(_) -> failwith "Mutual recursion not supported"
  | Texp_function{param; cases = [{c_rhs; _}]; _} ->
    (match Types.get_desc e.exp_type with
    | Tarrow (_, arg_ty, _, _) ->
      let arg_name = Smtcheck.create_var ctx.z3 ctx.ctr (Ident.name param) arg_ty in
      let arg_rty = RtyBase {base_ty = arg_ty; phi = Boolean.mk_true ctx.z3} in
      let ret_rty = type_infer ctx c_rhs in
      RtyArrow {arg_name; arg_rty; ret_rty}
    | _ -> failwith "OCaml type of function is not arrow")
  | Texp_function(_) -> failwith "keyword function not supported"
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
    let (arg_names, _arg_types, final_ty) = 
      List.fold_left 
        (fun (l1, l2, arg_rty) arg ->
          match arg_rty with
          | RtyArrow {arg_name; arg_rty; ret_rty} ->
            let ty' = type_check ctx arg arg_rty in
            (l1 @ [arg_name], l2 @ [ty'], ret_rty)
          | _ -> failwith "Not rty arrow")
        ([], [], ty) arg_exprs
    in
    let arg_z3_exprs = 
      List.map (fun arg -> Smtcheck.transl_expr ctx.z3 ctx.ctr arg) arg_exprs
    in
    let final_ty = 
      List.fold_left2
        (fun ty arg_name arg_expr -> subst ty arg_name arg_expr)
        final_ty arg_names arg_z3_exprs
    in
    final_ty
  | Texp_match(_) -> 
    RtyBase { base_ty = e.exp_type; phi = Boolean.mk_true ctx.z3 }
  | Texp_try(_)
  | Texp_tuple(_) -> failwith "NI type infer 3"
  | Texp_construct(_) ->
      (* only works for booleans *)
      let sort = Smtcheck.convert_type ctx.z3 ctx.ctr e.exp_type in
      RtyBase
        {
          base_ty = e.exp_type;
          phi = Boolean.mk_eq ctx.z3 
            (Expr.mk_const_s ctx.z3 "v" sort)
            (Smtcheck.transl_expr ctx.z3 ctx.ctr e)
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
  | Texp_ident (path, _, _) ->
      let name = Path.name path in
      let ty' = 
        (match ctx_lookup (ctx.rty @ ctx.curr) name with
        | None -> type_infer ctx e
        | Some (_, ty') -> ty')
      in
      (* Check if this is correct *)
      check_subtype e.exp_env ctx ("var_"^name, ty') ("v", ty)
  | Texp_apply(_)
  | Texp_constant(_) ->
    let ty' = type_infer ctx e in
    check_subtype e.exp_env ctx ("v", ty') ("v", ty)
  | Texp_function {param; cases = [{c_rhs; _}]; _} ->
    (match ty with
    | RtyBase (_) -> failwith "Type error: Function being analyzed with RtyBase type"
    | RtyArrow {arg_name; arg_rty; ret_rty;} -> 
      (* check that arg_name is a Z3 variable which has the same name as name *)
      if String.equal (Z3.Expr.to_string arg_name) (Ident.name param) then
        let new_ctx = {ctx with rty = (Ident.name param, arg_rty)::ctx.rty} in
        type_check new_ctx c_rhs ret_rty
      else
        failwith (Printf.sprintf "name mismatch for parameter %s and argument %s\n" 
          (Ident.name param) (Z3.Expr.to_string arg_name)))
  | Texp_let(_, [vb], expr) ->
    let rty1 = type_infer ctx vb.vb_expr in
    (match vb.vb_pat.pat_desc with
    | Tpat_var(ident, _) ->
      let name = Ident.name ident in
      let new_ctx = {ctx with rty = (name, rty1)::ctx.rty} in
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
      check_subtype e.exp_env ctx ("v", ty') ("v", ty)
    | _ -> failwith "Other constructors not supported")
  | Texp_ifthenelse(b, e1, e2o) ->
    let b_z3 = Smtcheck.transl_expr ctx.z3 ctx.ctr b in
    let ty1 = RtyBase {base_ty = Predef.type_int; phi = b_z3} in
    let new_ctx1 = {ctx with rty = ("", ty1)::ctx.rty} in
    type_check new_ctx1 e1 ty;
    (match e2o with
    | None -> ()
    | Some e2 -> 
      let neg_b_z3 = Boolean.mk_not ctx.z3 b_z3 in
      let ty2 = RtyBase{base_ty = Predef.type_int; phi = neg_b_z3} in
      let new_ctx2 = {ctx with rty = ("", ty2)::ctx.rty} in
      type_check new_ctx2 e2 ty)
  | Texp_variant(_)
  | Texp_record(_)
  | Texp_field(_)
  | Texp_setfield(_)
  | Texp_array(_)
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

(* type check or type infer the item given and give the updated ctx *)
let type_item (ctx: full_ctx) (prefix: string) (item: Typedtree.structure_item) : full_ctx =
  match item.str_desc with
  | Tstr_eval (e, _) -> let _ = type_infer ctx e in ctx
  | Tstr_value (_, [vb]) ->
    (let pty = ctx_pat_lookup ctx.rty prefix vb.vb_pat in
    match pty with
    | None ->
      let rty = type_infer ctx vb.vb_expr in
      let name = get_pat_str vb.vb_pat in
      { ctx with curr = (name, rty)::ctx.curr }
      (*{z3 = ctx.z3; rty = (prefix ^ name, rty)::ctx.rty}*)
    | Some (_, ty) -> type_check ctx vb.vb_expr ty; ctx)
  | _ -> ctx

let bidirect_type 
  (z3_ctx: Z3.context)
  (rctx: rty_ctx)
  (cctx: Smtcheck.constr_ctx)
  (prefix: string)
  (struc: Typedtree.structure) : ret_ctx =
    let fctx = List.fold_left
      (fun ctx item -> type_item ctx prefix item)
      {z3 = z3_ctx; rty = rctx; curr = []; ctr = cctx} struc.str_items
    in
    let pref_curr = List.map (fun (name, ty) -> (prefix ^ name, ty)) fctx.curr in
    {z3 = fctx.z3; rty = fctx.rty @ pref_curr}
