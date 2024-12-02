(* intermediate variable pass *)
let[@rty] foo = (v > 0 : int);;

let foo =
  let a = 4 in
  let b = a + 1 in
  b;;

foo
(* pass *)
