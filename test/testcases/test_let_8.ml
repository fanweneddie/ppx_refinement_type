(* more complex bindings fail *)
let[@rty] foo = (v > 0 : int);;

let foo =
  let a = 1 in
  let b = (let c = -3 in c + a) in
  b;;

foo
(* fail *)
