open Ocaml_common
open Rty
open Z3

(* Helper function to convert constant to Z3 Expr *)
let z3_expr_of_constant (z3_ctx: Z3.context) (c: Parsetree.constant) (* (bt: base_type)*) : Z3.Expr.expr =
  match c with
  | Pconst_integer (s, _) -> 
    (* Parse the string to integer, ignoring the suffix for simplicity *)
    let n = int_of_string s in
    Arithmetic.Integer.mk_numeral_i z3_ctx n
  | Pconst_char _ ->
      failwith "Unsupported constant type 1"
  | Pconst_string (_, _, _) ->
      failwith "Unsupported constant type 2"
  | Pconst_float (_, _) ->
      failwith "Unsupported constant type 3"
  (* Handle other constants accordingly *)

(* Function to translate Parsetree.expression to Z3 Expr *)
let rec translate_phi (z3_ctx: Z3.context) (phi_expr: Parsetree.expression) : Z3.Expr.expr =
  match phi_expr.pexp_desc with
  | Pexp_constant c ->
      (* Translate constants directly *)
      z3_expr_of_constant z3_ctx c
  | Pexp_apply (op_expr, args) ->
    (* Handle binary or unary operators *)
    let op =
      match op_expr.pexp_desc with
      | Pexp_ident {txt = Longident.Lident op; _} -> op
      | _ -> failwith "Unsupported operator expression"
    in
    let arg_exprs = List.map (fun (_, arg) -> translate_phi z3_ctx arg) args in
    (match op, arg_exprs with
     | "+", [lhs; rhs] -> Arithmetic.mk_add z3_ctx [lhs; rhs]
     | "-", [lhs; rhs] -> Arithmetic.mk_sub z3_ctx [lhs; rhs]
     | "*", [lhs; rhs] -> Arithmetic.mk_mul z3_ctx [lhs; rhs]
     | "/", [lhs; rhs] -> Arithmetic.mk_div z3_ctx lhs rhs
     | "&&", [lhs; rhs] -> Boolean.mk_and z3_ctx [lhs; rhs]
     | "||", [lhs; rhs] -> Boolean.mk_or z3_ctx [lhs; rhs]
     | "=", [lhs; rhs] -> Boolean.mk_eq z3_ctx lhs rhs
     | "<", [lhs; rhs] -> Arithmetic.mk_lt z3_ctx lhs rhs
     | ">", [lhs; rhs] -> Arithmetic.mk_gt z3_ctx lhs rhs
     | "<=", [lhs; rhs] -> Arithmetic.mk_le z3_ctx lhs rhs
     | ">=", [lhs; rhs] -> Arithmetic.mk_ge z3_ctx lhs rhs
     | "-", [arg] -> Arithmetic.mk_unary_minus z3_ctx arg
     | "not", [arg] -> Boolean.mk_not z3_ctx arg
     | _ -> failwith "Unsupported operator")
  | _ -> failwith "Unsupported expression in translate_phi 2"

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

and type_check (_ctx: rty_ctx) (e: Typedtree.expression) (_ty: rty): unit =
  match e.exp_desc with
  | Texp_ident(_) -> failwith "NI"
  | Texp_constant(_) ->
      (match _ty with
        | RtyBase {base_ty=bty; phi=_phi} ->
          if Rty.eq_base_type bty e.exp_type then begin
            (* call z3, convert constant c into predicate v = c, and show forall v, [(v == c) => phi]*)
            Printf.printf "Call z3 here\n";
            let () =
              (* Initialize Z3 context with default parameters *)
              let cfg = [("model", "false")] in
              let z3_ctx = Z3.mk_context cfg in
          
              (* Map your base types to Z3 sorts *)
              let z3_sort = 
                match bty.ptyp_desc with
                | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> Z3.Arithmetic.Integer.mk_sort z3_ctx
                (* | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> Z3.Boolean.mk_sort z3_ctx
                | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> Z3.Arithmetic.Real.mk_sort z3_ctx *)
                | _ -> failwith "Unsupported base type in type_check"
              in
          
              (* Create a variable v of the given sort *)
              let v = Z3.Expr.mk_const z3_ctx (Z3.Symbol.mk_string z3_ctx "v") z3_sort in
          
              (* Convert the constant c to a Z3 expression *)
              let c =
                match e.exp_desc with
                | Texp_constant (Const_int n) -> Z3.Arithmetic.Integer.mk_numeral_i z3_ctx n
                (* | Texp_constant (Const_bool b) -> Z3.Boolean.mk_val z3_ctx b
                | Texp_constant (Const_real r) -> Z3.Arithmetic.Real.mk_numeral z3_ctx (string_of_float r) z3_sort *)
                (* Handle other constant types accordingly *)
                | _ -> failwith "Unsupported constant type"
              in
          
              (* Translate phi into a Z3 expression *)
              let phi_z3 = translate_phi z3_ctx _phi in
          
              (* Create the implication (v == c) => phi *)
              let v_eq_c = Z3.Boolean.mk_eq z3_ctx v c in
              let implication = Z3.Boolean.mk_implies z3_ctx v_eq_c phi_z3 in
          
              (* Create a universally quantified formula: forall v, (v == c) => phi *)
              let quantifier = 
                Z3.Quantifier.mk_forall_const 
                  z3_ctx 
                  [] 
                  implication 
                  None 
                  [] 
                  [] 
                  None
                  None
              in
          
              (* Create a solver *)
              let solver = Z3.Solver.mk_solver z3_ctx None in

              let quantifier_expr = Quantifier.expr_of_quantifier quantifier in
          
              (* Assert the negation of the implication to check for validity *)
              Z3.Solver.add solver [Z3.Boolean.mk_not z3_ctx quantifier_expr];
          
              (* Check satisfiability *)
              match Z3.Solver.check solver [] with
              | SATISFIABLE ->
                  failwith "Type check failed: There exists a v where (v == c) and not phi"
              | UNSATISFIABLE ->
                  Printf.printf "Type check succeeded: The implication holds for all v\n"
              | UNKNOWN ->
                  failwith "Type check result is unknown"
            in
            () end
          else
            failwith "Type error at constant, base type not equal to expression type"
          
        | _ -> failwith "Type error not RtyBase for constant expression")
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

let type_infer_item (ctx: rty_ctx) (item: Typedtree.structure_item) : rty_exp option =
  match item.str_desc with
  | Tstr_eval (_e, _) -> None
  | Tstr_value (_, [vb]) ->
    (let pty = Rty.ctx_lookup ctx vb.vb_pat in
    match pty with
    | None -> None
    | Some (_, ty) -> type_check ctx vb.vb_expr ty; None)
  | _ -> None

let bidirect_type_infer (ctx: rty_ctx) (struc: Typedtree.structure) 
  (_ty: rty option) : rty_exp_list =
    List.filter_map
    (fun item -> type_infer_item ctx item)
    struc.str_items 
