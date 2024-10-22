open Ocaml_common
open Types
open Typedtree

(* arg_name being type string might not be good *)
type rty =
  | RtyBase of { base_ty : type_expr; phi : Z3.Expr.expr }
  | RtyArrow of { arg_name : string; arg_rty : rty; ret_rty : rty }

type rty_ctx = (string * rty) list

type rty_exp = (expression * rty)
type rty_exp_list = rty_exp list

let rec layout_rty = function
  | RtyBase { base_ty; phi } ->
      Printf.sprintf "{v: %s | %s}"
        (Ocaml_helper.string_of_type_expr base_ty)
        (Z3.Expr.to_string phi)
  | RtyArrow { arg_name; arg_rty; ret_rty } ->
      Printf.sprintf "%s:%s -> %s"
        arg_name
        (layout_rty arg_rty) (layout_rty ret_rty)
