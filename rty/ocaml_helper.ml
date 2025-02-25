open Ocaml_common

let unify_base_type (env: Env.t) (ty: Types.type_expr) (ty': Types.type_expr): bool =
  try 
    Ctype.unify env ty ty';
    true
  with
  | _ -> false

let string_of_pattern pattern =
  let _ = Format.flush_str_formatter () in
  Pprintast.pattern Format.str_formatter pattern;
  Format.flush_str_formatter ()

let string_of_type_expr ty =
  let _ = Format.flush_str_formatter () in
  Format.fprintf Format.str_formatter "%a"
  Printtyp.type_expr ty;
  Format.flush_str_formatter ()

let string_of_raw_type_expr ty =
  let _ = Format.flush_str_formatter () in
  Format.fprintf Format.str_formatter "%a"
  Printtyp.raw_type_expr ty;
  Format.flush_str_formatter ()

let string_of_typedtree expr =
  let _ = Format.flush_str_formatter () in
  Format.fprintf Format.str_formatter "%a"
  Printtyped.implementation expr;
  Format.flush_str_formatter ()
