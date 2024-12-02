(* more complex bindings pass *)
let[@rty] foo = (v > 0 : int);;

let foo =
  let a = 1 in
  let b = (let c = 2 in c + a) in
  b + 1;;

foo
(* pass *)
