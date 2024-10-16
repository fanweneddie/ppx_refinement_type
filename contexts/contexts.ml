open Rty

(* This module encapsulates both z3 context and refinement type context *)
let z3_ctx : Z3.context = Z3.mk_context []

let rtys_ctx: rty_ctx = []