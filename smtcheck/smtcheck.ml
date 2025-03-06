open Ocaml_common
open Typedtree
open Z3

type constr_ctx = (string * Sort.sort) list

let constr_lookup (cctx: constr_ctx) (ident: string): Sort.sort =
  print_endline (string_of_int (List.length cctx));
  let res = 
    List.find_opt (fun (name, _) -> String.equal name ident) cctx 
  in
  match res with
  | Some((_, sort)) -> sort
  | None -> failwith ("Constructor type " ^ (ident) ^ " not found")

let convert_type 
  (ctx: Z3.context) 
  (cctx: constr_ctx) 
  (bty: Types.type_expr): Sort.sort = 
  match Types.get_desc bty with
  | Tconstr(Pident(id), _, _) ->
    let name = Ident.name id in
    (match name with
    | "int" -> Arithmetic.Integer.mk_sort ctx
    | "bool" -> Boolean.mk_sort ctx
    | _ -> constr_lookup cctx name)
  | _ -> failwith "Type not constructor" 

let convert_constant 
  (ctx: Z3.context) 
  (c: Asttypes.constant): Expr.expr =
  match c with
  | Const_int n -> Arithmetic.Integer.mk_numeral_i ctx n
  | _ -> failwith "Unsupported constant type"

let create_var 
  (ctx: Z3.context) 
  (cctx: constr_ctx)
  (name: string) 
  (bty: Types.type_expr): Expr.expr =
  let sort = convert_type ctx cctx bty in
  Expr.mk_const_s ctx name sort

let rec transl_expr 
  (ctx: Z3.context) 
  (cctx: constr_ctx) 
  (e: expression): Z3.Expr.expr =
  match e.exp_desc with
  | Texp_ident (path, _, _) ->
    let name = "var_" ^ Path.name path in
    let sort = convert_type ctx cctx e.exp_type in
    Expr.mk_const_s ctx name sort
    (*Arithmetic.Integer.mk_const_s ctx name*)
  | Texp_constant c -> convert_constant ctx c
  | Texp_apply (op_expr, args) ->
    (let op: string =
      match op_expr.exp_desc with
      | Texp_ident (_, {txt=Longident.Lident op; _}, _) -> op
      | _ -> failwith "Unsupported operator expression"
    in
    let args_z3 = 
      List.filter_map 
        (fun (_, arg) ->
          match arg with
          | None -> None
          | Some arg -> Some (transl_expr ctx cctx arg)) 
        args 
    in
    match op, args_z3 with 
    | "-", [arg] -> Arithmetic.mk_unary_minus ctx arg
    | "not", [arg] -> Boolean.mk_not ctx arg
    | "=", [lhs; rhs] -> Boolean.mk_eq ctx lhs rhs
    | "/", [lhs; rhs] -> Arithmetic.mk_div ctx lhs rhs
    | "<", [lhs; rhs] -> Arithmetic.mk_lt ctx lhs rhs
    | ">", [lhs; rhs] -> Arithmetic.mk_gt ctx lhs rhs
    | "<=", [lhs; rhs] -> Arithmetic.mk_le ctx lhs rhs
    | ">=", [lhs; rhs] -> Arithmetic.mk_ge ctx lhs rhs
    | "+", _ -> Arithmetic.mk_add ctx args_z3
    | "-", _ -> Arithmetic.mk_sub ctx args_z3
    | "*", _ -> Arithmetic.mk_mul ctx args_z3
    | "&&", _ -> Boolean.mk_and ctx args_z3
    | "||", _ -> Boolean.mk_or ctx args_z3
    | _ ->
      let arg_sorts = 
        List.filter_map 
          (fun (_, arg) -> 
            match arg with
            | None -> failwith "transl_expr: Labelled partial application not supported"
            | Some arg -> Some (convert_type ctx cctx arg.exp_type)) 
          args 
      in
      let ret_sort = convert_type ctx cctx e.exp_type in
      let f = FuncDecl.mk_func_decl_s ctx op arg_sorts ret_sort in
      FuncDecl.apply f args_z3) 
  | Texp_construct (_, {cstr_name; _}, _) ->
    (match cstr_name with
    | "true" -> Boolean.mk_true ctx
    | "false" -> Boolean.mk_false ctx
    | _ -> failwith ("Unsupported constructor: " ^ cstr_name))
  | _ -> failwith "transl_expr NI"


let convert_phi (ctx: context) (phi: expression): Z3.Expr.expr =
  transl_expr ctx [] phi

let check (ctx: Z3.context) (c: Expr.expr) : unit =
  let solver = Z3.Solver.mk_solver ctx None in 
  let subtype_expr = Boolean.mk_not ctx c in
  let _ = Solver.add solver [subtype_expr] in 
  match Solver.check (solver) [] with
  | SATISFIABLE -> failwith "Type error"
  | UNSATISFIABLE -> ()
  | UNKNOWN -> failwith "Z3 unknown"
