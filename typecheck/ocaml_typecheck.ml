open Ocaml_common

let initial_env () =
  let () = Compmisc.init_path () in
  let initially_opened_module =
    if !Clflags.nopervasives then None else Some "Stdlib"
  in
  let open_implicit_modules = [] in
  Typemod.initial_env
    ~loc:(Location.in_file "ocamldoc command line")
    ~open_implicit_modules ~initially_opened_module ~safe_string:true

let process_implementation_file parsetree =
  let env = initial_env () in
  try
    let typedtree = Typemod.type_implementation "" "" "" env parsetree in
    typedtree
  with
  | Syntaxerr.Error _ as exn ->
      (match Location.error_of_exn exn with
      | Some (`Ok err) -> Location.print_report Format.err_formatter err
      | _ -> assert false);
      assert false
  | Failure s ->
      prerr_endline s;
      assert false

let process_expr (env: Env.t) 
  (expr: Parsetree.expression): Typedtree.expression =
  Typecore.type_expression env expr

let process_type (env: Env.t) (ty: Parsetree.core_type): Types.type_expr = 
  (Typetexp.transl_simple_type env false ty).ctyp_type

let create_val_desc (ty: Types.type_expr): Types.value_description =
  { val_type = ty;
    val_kind = Val_reg;
    val_loc = Location.none;
    val_attributes = [];
    val_uid = Types.Uid.internal_not_actually_unique;
  }