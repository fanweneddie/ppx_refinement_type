open Ocaml_common
open Types
open Typedtree
open Z3

(* arg_name being type Expr.expr might not be good *)
type rty =
  | RtyBase of { base_ty : type_expr; phi : Expr.expr }
  | RtyArrow of { arg_name : Expr.expr; arg_rty : rty; ret_rty : rty }

type rty_ctx = (string * string * rty) list

type rty_exp = (expression * rty)
type rty_exp_list = rty_exp list

let rec layout_rty = function
  | RtyBase { base_ty; phi } ->
      Printf.sprintf "{v: %s | %s}"
        (Ocaml_helper.string_of_type_expr base_ty)
        (Z3.Expr.to_string phi)
  | RtyArrow { arg_name; arg_rty; ret_rty } ->
      Printf.sprintf "%s:%s -> %s"
        (Z3.Expr.to_string arg_name)
        (layout_rty arg_rty) (layout_rty ret_rty)

let print_rty_ctx ctx = 
  List.fold_left 
    (fun _ (name, prefix, ty) -> 
      (print_endline (prefix ^ name); print_endline (layout_rty ty))) 
    () ctx

let rec rty_to_type_expr (rty: rty): Types.type_expr =
  match rty with
  | RtyBase { base_ty; _ } -> base_ty
  | RtyArrow { arg_rty; ret_rty; _ } ->
    let arg_ty = rty_to_type_expr arg_rty in
    let ret_ty = rty_to_type_expr ret_rty in
    Types.create_expr 
      (Tarrow (Nolabel, arg_ty, ret_ty, Types.commu_ok))
      ~level:0 ~scope:0 ~id:0


module Builtin = struct
  let plus (ctx: Z3.context): rty = 
    let x = Arithmetic.Integer.mk_const_s ctx "var_x" in
    let y = Arithmetic.Integer.mk_const_s ctx "var_y" in
    let v = Arithmetic.Integer.mk_const_s ctx "v" in
    let phi' = Boolean.mk_eq ctx v (Arithmetic.mk_add ctx [x; y]) in
    RtyArrow {
      arg_name = x;
      arg_rty = RtyBase {
        base_ty = Predef.type_int; phi = Boolean.mk_true ctx
      };
      ret_rty = RtyArrow {
        arg_name = y;
        arg_rty = RtyBase {
          base_ty = Predef.type_int; phi = Boolean.mk_true ctx
        };
        ret_rty = RtyBase {
          base_ty = Predef.type_int; phi = phi'
        }
      }
    }

  let minus (ctx: Z3.context): rty =
    let x = Arithmetic.Integer.mk_const_s ctx "var_x" in
    let y = Arithmetic.Integer.mk_const_s ctx "var_y" in
    let v = Arithmetic.Integer.mk_const_s ctx "v" in
    let phi' = Boolean.mk_eq ctx v (Arithmetic.mk_sub ctx [x; y]) in
    RtyArrow {
      arg_name = x;
      arg_rty = RtyBase {
        base_ty = Predef.type_int; phi = Boolean.mk_true ctx
      };
      ret_rty = RtyArrow {
        arg_name = y;
        arg_rty = RtyBase {
          base_ty = Predef.type_int; phi = Boolean.mk_true ctx
        };
        ret_rty = RtyBase {
          base_ty = Predef.type_int; phi = phi'
        }
      }
    } 

  let times (ctx: Z3.context): rty =
    let x = Arithmetic.Integer.mk_const_s ctx "var_x" in 
    let y = Arithmetic.Integer.mk_const_s ctx "var_y" in
    let v = Arithmetic.Integer.mk_const_s ctx "v" in
    let phi' = Boolean.mk_eq ctx v (Arithmetic.mk_mul ctx [x; y]) in
    RtyArrow {
      arg_name = x;
      arg_rty = RtyBase {
        base_ty = Predef.type_int; phi = Boolean.mk_true ctx
      };
      ret_rty = RtyArrow {
        arg_name = y;
        arg_rty = RtyBase {
          base_ty = Predef.type_int; phi = Boolean.mk_true ctx
        };
        ret_rty = RtyBase {
          base_ty = Predef.type_int; phi = phi'
        }
      }
    }

  let or_op (ctx: Z3.context): rty = 
    let x = Boolean.mk_const_s ctx "var_x" in
    let y = Boolean.mk_const_s ctx "var_y" in
    let v = Boolean.mk_const_s ctx "v" in
    let phi' = Boolean.mk_eq ctx v (Boolean.mk_or ctx [x; y]) in
    RtyArrow {
      arg_name = x;
      arg_rty = RtyBase {
        base_ty = Predef.type_bool; phi = Boolean.mk_true ctx
      };
      ret_rty = RtyArrow {
        arg_name = y;
        arg_rty = RtyBase {
          base_ty = Predef.type_bool; phi = Boolean.mk_true ctx
        };
        ret_rty = RtyBase {
          base_ty = Predef.type_bool; phi = phi'
        }
      }
    }
  
  let and_op (ctx: Z3.context): rty = 
    let x = Boolean.mk_const_s ctx "var_x" in
    let y = Boolean.mk_const_s ctx "var_y" in
    let v = Boolean.mk_const_s ctx "v" in
    let phi' = Boolean.mk_eq ctx v (Boolean.mk_and ctx [x; y]) in
    RtyArrow {
      arg_name = x;
      arg_rty = RtyBase {
        base_ty = Predef.type_bool; phi = Boolean.mk_true ctx
      };
      ret_rty = RtyArrow {
        arg_name = y;
        arg_rty = RtyBase {
          base_ty = Predef.type_bool; phi = Boolean.mk_true ctx
        };
        ret_rty = RtyBase {
          base_ty = Predef.type_bool; phi = phi'
        }
      }
    }

  (* Need to remove this later *)
  let equal (ctx: Z3.context): rty =
    let x = Arithmetic.Integer.mk_const_s ctx "var_x" in 
    let y = Arithmetic.Integer.mk_const_s ctx "var_y" in
    let v = Boolean.mk_const_s ctx "v" in
    let phi' = Boolean.mk_eq ctx v (Boolean.mk_eq ctx x y) in
    RtyArrow {
      arg_name = x;
      arg_rty = RtyBase {
        base_ty = Predef.type_int; phi = Boolean.mk_true ctx
      };
      ret_rty = RtyArrow {
        arg_name = y;
        arg_rty = RtyBase {
          base_ty = Predef.type_int; phi = Boolean.mk_true ctx
        };
        ret_rty = RtyBase {
          base_ty = Predef.type_bool; phi = phi'
        }
      }
    } 
  
  let gt (ctx: Z3.context): rty =
    let x = Arithmetic.Integer.mk_const_s ctx "var_x" in 
    let y = Arithmetic.Integer.mk_const_s ctx "var_y" in
    let v = Boolean.mk_const_s ctx "v" in
    let phi' = Boolean.mk_eq ctx v (Arithmetic.mk_gt ctx x y) in
    RtyArrow {
      arg_name = x;
      arg_rty = RtyBase {
        base_ty = Predef.type_int; phi = Boolean.mk_true ctx
      };
      ret_rty = RtyArrow {
        arg_name = y;
        arg_rty = RtyBase {
          base_ty = Predef.type_int; phi = Boolean.mk_true ctx
        };
        ret_rty = RtyBase {
          base_ty = Predef.type_bool; phi = phi'
        }
      }
    } 
 
  let add_builtins (ctx: Z3.context) (rctx: rty_ctx): rty_ctx =
    [("Stdlib.+", "", plus ctx); 
    ("Stdlib.-", "", minus ctx); 
    ("Stdlib.*", "", times ctx);
    ("Stdlib.=", "", equal ctx);
    ("Stdlib.>", "", gt ctx);
    ("Stdlib.||", "", or_op ctx);
    ("Stdlib.&&", "", and_op ctx)] @ rctx
end
