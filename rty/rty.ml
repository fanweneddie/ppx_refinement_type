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


(*module Builtin = struct
end*)
