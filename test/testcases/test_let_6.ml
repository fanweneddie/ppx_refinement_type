(* intermediate variable fail *)
let[@rty] foo = (v > 0 : int);;

let foo =
  let a = 4 in
  let b = a - 5 in
  b;;

foo
(* fail *)
