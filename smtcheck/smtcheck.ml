open Ocaml_common
open Parsetree
open Rty
open Z3

(* very complicated *)
let convert_type (ctx: context) (bty: core_ty): Z3.Sort.sort = 
  failwith "NI"
  (*match bty.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> Z3.Arithmetic.Integer.mk_sort ctx
  (* | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> Z3.Boolean.mk_sort ctx
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> Z3.Arithmetic.Real.mk_sort ctx *)
  | _ -> failwith "Unsupported base type in type_check"*)

let convert_constant (ctx: context) (c: constant): Z3.Expr.expr =
  match c with
  | Pconst_integer(value, _) ->
      Arithmetic.mk_numeral_s ctx value
  | _ -> failwith "convert_constant NI"

(* we shall assume variables are integers
we would need the ocaml environment, assign v the
base type, and add it to the environment
typecheck the expression phi
Then use the environment in this function to
assign variables to the correct sorts 
This would be very complicated *)
let rec transl_expr (ctx: context) (e: expression) =
  match phi with
  | Pexp_ident {txt=Longident.Lident name; _} -> 
      Arithmetic.Integer.mk_const_s ctx name
  | Pexp_constant c -> convert_constant ctx c
  | Pexp_apply (op_expr, args) ->
    (let op: string =
      match op_expr.pexp_desc with
      | Pexp_ident {txt = Longident.Lident op; _} -> op
      | _ -> failwith "Unsupported operator expression"
    in
    let arg_exprs = List.map (fun (_, arg) -> transl_expr ctx arg) args in
    match op, args with 
     | "+", _ -> Arithmetic.mk_add ctx args
     | "-", _ -> Arithmetic.mk_sub ctx args
     | "*", _ -> Arithmetic.mk_mul ctx args
     | "&&", _ -> Boolean.mk_and ctx args
     | "||", _ -> Boolean.mk_or ctx args
     | "=", [lhs; rhs] -> Boolean.mk_eq ctx lhs rhs
     | "/", [lhs; rhs] -> Arithmetic.mk_div ctx lhs rhs
     | "<", [lhs; rhs] -> Arithmetic.mk_lt ctx lhs rhs
     | ">", [lhs; rhs] -> Arithmetic.mk_gt ctx lhs rhs
     | "<=", [lhs; rhs] -> Arithmetic.mk_le ctx lhs rhs
     | ">=", [lhs; rhs] -> Arithmetic.mk_ge ctx lhs rhs
     | "-", [arg] -> Arithmetic.mk_unary_minus ctx arg
     | "not", [arg] -> Boolean.mk_not ctx arg
     | _ -> failwith "Unsupported operator")
  | _ -> failwith "transl_expr NI"


let convert_phi (ctx: context) (phi: expression): Z3.Expr.expr =
  transl_expr ctx phi

let handle_const (value: Asttypes.constant) (bty: core_type) (phi: expression): unit =
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
  failwith "NI"
