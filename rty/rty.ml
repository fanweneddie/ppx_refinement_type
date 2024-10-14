open Ocaml_common
open Parsetree

type rty =
  | RtyBase of { base_ty : core_type; phi : expression }
  | RtyArrow of { arg_name : pattern; arg_rty : rty; ret_rty : rty }

type rty_ctx = (pattern * rty) list

type rty_exp = (Typedtree.expression * rty)
type rty_exp_list = rty_exp list

let ctx_lookup (ctx: rty_ctx) (pat: Typedtree.pattern): (pattern * rty) option =
    match pat.pat_desc with
    | Tpat_var(_, {txt=pat_name; _}) ->
      List.find_opt
        (fun (p, _) ->
          match p.ppat_desc with
          | Ppat_var({txt=name; _}) -> name = pat_name
          | _ -> false)
        ctx
    | _ -> None

let eq_base_type (ty: core_type) (ty': Types.type_expr): bool =
  try 
    Ctype.unify Env.initial_safe_string 
      ((Typetexp.transl_simple_type Env.initial_safe_string false ty).ctyp_type)
      ty'; 
    true
  with
  | _ -> false

(*module Builtin = struct
end*)
