(* multiple let bindings pass *)
let[@rty] foo = (v > 0 : int);;

let foo =
  let a = 2 in
  let b = 3 in
  a + b;;

foo
(* pass *)
