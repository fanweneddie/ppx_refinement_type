open Ocaml_common
open Typedtree
open Z3

(* very complicated *)
let convert_type (ctx: Z3.context) (bty: Types.type_expr): Z3.Sort.sort = 
  match Types.get_desc bty with
  | Tconstr(_, [expr], _) -> (
    match Types.get_desc expr with
    | Tvar(Some "int") -> Arithmetic.Integer.mk_sort ctx
    | _ -> failwith "NI CONVERT_TYPE for function (todo)"
  )
  | _ -> failwith "Unsupported constant types" 

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
  | Texp_construct (_, {cstr_name; _}, _args) ->
    (match cstr_name with
    | "true" -> Boolean.mk_true ctx
    | "false" -> Boolean.mk_false ctx
    | _ -> failwith ("Unsupported constructor: " ^ cstr_name))
  | _ -> failwith "transl_expr NI"


let convert_phi (ctx: context) (phi: expression): Z3.Expr.expr =
  transl_expr ctx phi

let check (ctx: Z3.context) (phi1: Expr.expr) (phi2: Expr.expr) : unit = 
  let solver = Z3.Solver.mk_solver ctx None in 
  let subtype_expr = Boolean.mk_not ctx (Boolean.mk_implies ctx phi1 phi2) in
  let () = Solver.add solver [subtype_expr] in 
  match Solver.check (solver) [] with
  | SATISFIABLE -> failwith "Type error"
  | UNSATISFIABLE -> ()
  | UNKNOWN -> failwith "Z3 unknown"
