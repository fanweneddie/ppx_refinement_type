open Ocaml_common
open Typedtree

let genSym =
  let c = ref 0
  in fun () -> c := !c + 1; "G" ^ (string_of_int !c)

type cont = Typedtree.expression -> Typedtree.expression
type conts = Typedtree.expression list -> Typedtree.expression

let base_cont = fun (e: Typedtree.expression) -> e

let isValue (e: Typedtree.expression): bool =
  match e.exp_desc with
  | Texp_ident(_) 
  | Texp_constant(_) -> true
  | Texp_construct(_, {cstr_name; _}, _) ->
    (match cstr_name with
    | ("true" | "false") -> true
    | _ -> false)
  | _ -> false

let rec normalize_name (e: Typedtree.expression) (k: cont): Typedtree.expression = 
  normalize_exp(e)
  (
    fun e' -> 
      if (isValue e')
        then (k e')
      else let t = genSym() in
      let pat: Typedtree.pattern =  
      {
        pat_desc = Tpat_var(Ident.create_local(t), {txt = t; loc = Location.none});
        pat_loc = Location.none;
        pat_extra = [];
        pat_type = e'.exp_type;
        pat_env = Env.empty;
        pat_attributes = [];
      }
      in
      let new_vb: Typedtree.value_binding = {
        vb_pat = pat;
        vb_expr = e';
        vb_attributes = [];
        vb_loc = Location.none;
      }
      in
      let new_ident: Typedtree.expression_desc = 
      Texp_ident(
        Pident (Ident.create_local(t)),
        {txt = Longident.Lident t; loc = Location.none},
        {
          val_type = e'.exp_type;
          val_kind = Val_reg;
          val_loc = Location.none;
          val_attributes = [];
          val_uid = Shape.Uid.internal_not_actually_unique;
        }
      )
      in
      let new_body: Typedtree.expression = {
        exp_desc = new_ident;
        exp_loc = e'.exp_loc;
        exp_extra = e'.exp_extra;
        exp_type = e'.exp_type;
        exp_env = e'.exp_env;
        exp_attributes = e'.exp_attributes;
      }
      in
      let new_rest = k new_body in
       {
        exp_desc = Texp_let(Nonrecursive, [new_vb], new_rest); 
        exp_loc = e'.exp_loc;
        exp_extra = e'.exp_extra;
        exp_type = (new_rest).exp_type;
        exp_env = e'.exp_env;
        exp_attributes = e'.exp_attributes;
       }
  )

and normalize_names (es : Typedtree.expression list) (k: conts) = 
  match es with
  | [] -> k []
  | e :: es' -> normalize_name e (fun t -> normalize_names es' (fun ts -> k (t :: ts)))

and normalize_exp (e : Typedtree.expression) (k: cont): Typedtree.expression =
  match e.exp_desc with
  | Texp_ident(_) 
  | Texp_constant(_) -> k e
  | Texp_construct(_, {cstr_name; _}, _) ->
    (match cstr_name with
    | ("true" | "false") -> k e
    | _ -> e) 
  | Texp_let(flag, [vb], expr) ->
    normalize_exp vb.vb_expr ( fun anf_rhs ->
      let new_vb = {vb with vb_expr = anf_rhs} in
      let new_let = Texp_let (flag, [new_vb], normalize_exp expr k) in
      {e with exp_desc = new_let}
    ) 
  | Texp_apply(op, args) -> 
    let arg_exprs = List.map 
    (fun arg -> 
      match arg with
      | (_, Some e) -> e 
      | _ -> failwith "Labelled partial ap not supported ANORMAL") args
    in
    normalize_name op ( fun anf_op ->
      normalize_names arg_exprs ( fun anf_args ->
        let anfs = List.map base_cont (anf_op :: anf_args) in
        let new_app = Texp_apply (List.hd anfs, List.map (fun arg -> (Asttypes.Nolabel, Some(arg))) (List.tl anfs) ) in 
        k ({e with exp_desc = new_app})
      )
    )
  | Texp_ifthenelse(cond, tbranch, ebranch_opt) -> 
    normalize_name cond ( fun anf_cond ->
      match ebranch_opt with
      | Some(ebranch) -> 
        let new_if = Texp_ifthenelse(anf_cond, 
          normalize_term tbranch,
          Some(normalize_term ebranch)
        ) in
        k {e with exp_desc = new_if}
      | None -> 
        let new_if = Texp_ifthenelse(anf_cond,
        normalize_term tbranch,
        None) in
        k {e with exp_desc = new_if}
    )
  | Texp_function {arg_label; param; cases; partial;} ->
    let anf_body = match List.hd cases with 
    | {c_rhs; _} -> normalize_term c_rhs
    in
    let new_hd = {(List.hd cases) with c_rhs = anf_body}
    in
    let new_fun = Texp_function {
      arg_label = arg_label; param = param; cases = new_hd :: List.tl cases; partial = partial; };
    in
    k {e with exp_desc = new_fun}
  | _ -> e

and normalize_term (e: Typedtree.expression) = 
  (* print_endline ("Expression is = " ^ 
  (Pprintast.string_of_expression @@ Ocaml_common.Untypeast.untype_expression e)); *)
  normalize_exp e base_cont

let normalize_item (item: Typedtree.structure_item): Typedtree.structure_item =
  match item.str_desc with
  | Tstr_eval(e, attr) -> {
    str_desc = Tstr_eval (normalize_term e, attr);
    str_loc = item.str_loc;
    str_env = item.str_env;
  }
  | Tstr_value(flag, [vb]) -> 
    let old_expr = vb.vb_expr in
    let new_vb = {vb with vb_expr = normalize_term old_expr} in
    {item with str_desc = Tstr_value(flag, [new_vb])}
  | _ -> item

let normalize (struc: Typedtree.structure) : Typedtree.structure =
  { 
   str_items = List.map (fun item -> normalize_item item) (struc.str_items);
   str_type = struc.str_type;
   str_final_env = struc.str_final_env;
  }
  
