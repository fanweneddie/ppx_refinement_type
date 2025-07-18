open Ocaml_common
open Typedtree
open Z3

type constr_ctx = (string * Sort.sort) list
(*type val_ctx = (string * string) list*)

let constr_lookup (cctx: constr_ctx) (ident: string): Sort.sort =
  let res = 
    List.find_opt (fun (name, _) -> (*print_endline name;*) String.equal name ident) cctx 
  in
  (*print_endline @@ string_of_int (List.length cctx);*)
  match res with
  | Some((_, sort)) -> sort
  | None -> failwith ("Constructor type " ^ (ident) ^ " not found")

let rec lookup_bound (l: string list) (ident: string) (ind: int): int option =
  match l with
  | [] -> None
  | hd::tl -> if hd = ident then Some ind else lookup_bound tl ident (ind+1)

let convert_type 
  (ctx: Z3.context) 
  (cctx: constr_ctx)
  (prefix: string)
  (bty: Types.type_expr): Sort.sort =
  match Types.get_desc bty with
  | Tconstr(path, _, _) ->
    let name = Path.name path in
    (match name with
    | "int" -> Arithmetic.Integer.mk_sort ctx
    | "bool" -> Boolean.mk_sort ctx
    | _ -> constr_lookup cctx (prefix ^ name))
  | Tarrow(_) -> failwith "convert_type: Tarrow not supported"
  | _ -> failwith "convert_type: Type not constructor" 

let convert_constant 
  (ctx: Z3.context) 
  (c: Asttypes.constant): Expr.expr =
  match c with
  | Const_int n -> Arithmetic.Integer.mk_numeral_i ctx n
  | _ -> failwith "convert_constant: Unsupported constant type"

(*let convert_val_name (vctx: val_ctx) (ident: string) =
  let res = 
    List.find_opt (fun (name, _) -> String.equal name ident) vctx 
  in
  match res with
  | Some((_, full_name)) -> full_name
  | None -> ident*)

let create_var 
  (ctx: Z3.context) 
  (cctx: constr_ctx)
  (prefix: string)
  (name: string) 
  (bty: Types.type_expr): Expr.expr =
  let sort = convert_type ctx cctx prefix bty in
  Expr.mk_const_s ctx name sort

let make_app
  (ctx: Z3.context)
  (op_name: string)
  (args: Expr.expr list)
  (arg_sorts: Sort.sort list)
  (ret_sort: Sort.sort): Expr.expr =
    let f = FuncDecl.mk_func_decl_s ctx op_name arg_sorts ret_sort in
    FuncDecl.apply f args

let rec transl_expr 
  (ctx: Z3.context) 
  (cctx: constr_ctx)
  (bound_vars: string list)
  (*(vctx: val_ctx)*)
  (prefix: string)
  (e: expression): Z3.Expr.expr =
  match e.exp_desc with
  | Texp_ident (path, _, _) ->
    (let name =
      (match path with
      | Pident(_) -> "var_" ^ prefix ^ Path.name path
      | _ -> "var_" ^ Path.name path)
    in
    let sort = convert_type ctx cctx prefix e.exp_type in
    match (lookup_bound bound_vars name 0) with
    | None -> Expr.mk_const_s ctx name sort
    | Some n -> Quantifier.mk_bound ctx n sort)
    (*Arithmetic.Integer.mk_const_s ctx name*)
  | Texp_constant c -> convert_constant ctx c
  | Texp_apply (op_expr, args) ->
    (let op: string =
      match op_expr.exp_desc with
      | Texp_ident (path, _, _) ->
        let name = Path.name path in
        (match path with
        | Pident(_) when not(String.equal name "#==>") -> prefix ^ Path.name path
        | _ -> Path.name path)
        (*let name = Longident.flatten ident.txt |>
          List.fold_left (fun acc x -> acc ^ x ^ ".") "" 
        in
        let name = String.sub name 0 (String.length name - 1) in*)
        (*convert_val_name vctx name*)
      | _ -> failwith "Smtcheck: Unsupported operator expression"
    in
    let args_z3 = 
      List.filter_map 
        (fun (_, arg) ->
          match arg with
          | None -> None
          | Some arg -> Some (transl_expr ctx cctx bound_vars (*vctx*) prefix arg))
        args 
    in
    match op, args_z3 with 
    | "Stdlib.-", [arg] -> Arithmetic.mk_unary_minus ctx arg
    | "Stdlib.not", [arg] -> Boolean.mk_not ctx arg
    | "Stdlib.=", [lhs; rhs] -> Boolean.mk_eq ctx lhs rhs
    | "Stdlib./", [lhs; rhs] -> Arithmetic.mk_div ctx lhs rhs
    | "Stdlib.<", [lhs; rhs] -> Arithmetic.mk_lt ctx lhs rhs
    | "Stdlib.>", [lhs; rhs] -> Arithmetic.mk_gt ctx lhs rhs
    | "Stdlib.<=", [lhs; rhs] -> Arithmetic.mk_le ctx lhs rhs
    | "Stdlib.>=", [lhs; rhs] -> Arithmetic.mk_ge ctx lhs rhs
    | "#==>", [lhs; rhs] -> Boolean.mk_implies ctx lhs rhs
    | "Stdlib.+", _ -> Arithmetic.mk_add ctx args_z3
    | "Stdlib.-", _ -> Arithmetic.mk_sub ctx args_z3
    | "Stdlib.*", _ -> Arithmetic.mk_mul ctx args_z3
    | "Stdlib.&&", _ -> Boolean.mk_and ctx args_z3
    | "Stdlib.||", _ -> Boolean.mk_or ctx args_z3
    | _ ->
      let arg_sorts = 
        List.filter_map 
          (fun (_, arg) -> 
            match arg with
            | None -> failwith "transl_expr: Labelled partial application not supported"
            | Some arg -> Some (convert_type ctx cctx prefix arg.exp_type)) 
          args 
      in
      let ret_sort = convert_type ctx cctx prefix e.exp_type in
      let f = FuncDecl.mk_func_decl_s ctx op arg_sorts ret_sort in
      FuncDecl.apply f args_z3) 
  | Texp_construct (_, {cstr_name; _}, _) ->
    (match cstr_name with
    | "true" -> Boolean.mk_true ctx
    | "false" -> Boolean.mk_false ctx
    | _ -> failwith ("Unsupported constructor: " ^ cstr_name))
  | _ -> failwith "transl_expr NI"


(*let convert_phi (ctx: context) (phi: expression): Z3.Expr.expr =
  transl_expr ctx [] phi*)

let check (ctx: Z3.context) (assumptions: Expr.expr list) (c: Expr.expr): unit =
  let solver = Z3.Solver.mk_solver ctx None in 
  let subtype_expr = Boolean.mk_not ctx c in
  let _ = Solver.add solver assumptions in
  let _ = Solver.add solver [subtype_expr] in
  (*print_endline "BEGIN";
  (*let _ = List.map (fun expr -> print_endline (Z3.Expr.to_string expr)) assumptions in
  print_endline (Z3.Expr.to_string c);*)
  print_endline (Z3.Solver.to_string solver);
  print_endline "END";*)
  match Solver.check (solver) [] with
  | SATISFIABLE -> 
    (let model = Solver.get_model solver in
    match model with
    | None -> failwith "No model";
    | Some(model) ->
      print_endline (Model.to_string model);
      failwith "Type error")
  | UNSATISFIABLE -> ()
  | UNKNOWN -> failwith "Z3 unknown"
