open Ocaml_common
open Rty

let bidirect_type_infer (_ctx: rty_ctx) (_struc: Typedtree.structure_item) 
  (ty: rty option) : rty option =
  match ty with
  | None -> failwith "NI"(* Type synthesis *)
  | Some _ty' -> failwith "NI" (* Type unify with ty and ty' and then do type check *) 

let type_infer (_ctx: rty_ctx) (struc: Typedtree.structure_item) : rty option =
  match struc.str_desc with
  | Tstr_value (_, _vb) -> failwith "NI" (* Regular type checking *)
  | _ -> failwith "NI" (* Figure out the rest*)
