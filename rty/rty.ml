open Parsetree

type rty =
  | RtyBase of { base_ty : core_type; phi : expression }
  | RtyArrow of { arg_name : pattern; arg_rty : rty; ret_rty : rty }

type rty_ctx = (pattern * rty) list
