(* multiple let bindings fail *)
let[@rty] foo = (v > 0 : int);;

let foo =
  let a = -3 in
  let b = 2 in
  a + b;;

foo
(* fail *)
