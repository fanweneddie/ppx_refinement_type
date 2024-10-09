open Ocaml_common
open Parsetree

type rty =
  | RtyBase of { base_ty : core_type; phi : expression }
  | RtyArrow of { arg_name : pattern; arg_rty : rty; ret_rty : rty }

type rty_ctx = (pattern * rty) list

type rty_exp = (Typedtree.expression * rty)
type rty_exp_list = rty_exp list

(*module Builtin = struct
end*)
