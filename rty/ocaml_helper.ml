open Ocaml_common

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
