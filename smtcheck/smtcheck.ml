open Ocaml_common
open Typedtree
open Z3

(* very complicated *)
let convert_type (_ctx: Z3.context) (_bty: Types.type_expr): Z3.Sort.sort = 
  failwith "NI"
  (*match bty.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> Z3.Arithmetic.Integer.mk_sort ctx
  (* | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> Z3.Boolean.mk_sort ctx
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> Z3.Arithmetic.Real.mk_sort ctx *)
  | _ -> failwith "Unsupported base type in type_check"*)

let convert_constant (ctx: Z3.context) (c: Asttypes.constant): Z3.Expr.expr =
  match c with
  | Const_int n -> Z3.Arithmetic.Integer.mk_numeral_i ctx n
  | _ -> failwith "Unsupported constant type"

let rec transl_expr (ctx: Z3.context) (e: expression): Z3.Expr.expr =
  match e.exp_desc with
  | Texp_ident (_, {txt=Longident.Lident name; _}, _) ->
    (* Take e.exp_type convert it to sort*)
    (* Create variable of the sort *)
    Arithmetic.Integer.mk_const_s ctx name
  | Texp_constant c -> convert_constant ctx c
  | Texp_apply (op_expr, args) ->
    (* this should be recursive call here *)
    (let op: string =
      match op_expr.exp_desc with
      | Texp_ident (_, {txt=Longident.Lident op; _}, _) -> op
      | _ -> failwith "Unsupported operator expression"
    in
    let args = 
      List.filter_map 
        (fun (_, arg) ->
          match arg with
          | None -> None
          | Some arg -> Some (transl_expr ctx arg)) 
        args 
    in
    match op, args with 
    | "-", [arg] -> Arithmetic.mk_unary_minus ctx arg
    | "not", [arg] -> Boolean.mk_not ctx arg
    | "=", [lhs; rhs] -> Boolean.mk_eq ctx lhs rhs
    | "/", [lhs; rhs] -> Arithmetic.mk_div ctx lhs rhs
    | "<", [lhs; rhs] -> Arithmetic.mk_lt ctx lhs rhs
    | ">", [lhs; rhs] -> Arithmetic.mk_gt ctx lhs rhs
    | "<=", [lhs; rhs] -> Arithmetic.mk_le ctx lhs rhs
    | ">=", [lhs; rhs] -> Arithmetic.mk_ge ctx lhs rhs
    | "+", _ -> Arithmetic.mk_add ctx args
    | "-", _ -> Arithmetic.mk_sub ctx args
    | "*", _ -> Arithmetic.mk_mul ctx args
    | "&&", _ -> Boolean.mk_and ctx args
    | "||", _ -> Boolean.mk_or ctx args
    | _ -> failwith "Unsupported operator")
  | _ -> failwith "transl_expr NI"


let convert_phi (ctx: context) (phi: expression): Z3.Expr.expr =
  transl_expr ctx phi

(*let handle_const (value: Asttypes.constant) (bty: core_type) (phi: expression): unit =
  let ctx = mk_context [] in
  let v = Arithmetic.Integer.mk_const_s ctx "v" in
  let c =
    match value with
    | Const_int n -> Z3.Arithmetic.Integer.mk_numeral_i ctx n
    | _ -> failwith "Unsupported constant type"
  in
  let phi_z3 = Smtcheck.convert_phi ctx phi in
  let v_eq_c = Z3.Boolean.mk_eq ctx v c in
  let implication = Z3.Boolean.mk_implies ctx v_eq_c phi_z3 in
  let solver = Z3.Solver.mk_solver ctx None in
  failwith "NI"*)
