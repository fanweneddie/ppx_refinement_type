(* multiple arguments, the return value doesn’t satisfy the constraint *)
let[@rty] baz = 
  let x = (v >= 1: int) in 
  let y = (v >= 1: int) in
  (v >= 2: int)

let baz x y = x + y - 1;;

baz 1 1;;
(* fail *)