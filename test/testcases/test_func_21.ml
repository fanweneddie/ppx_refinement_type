(* multiple arguments, not satisfying the constraint *)
let[@rty] baz2 = 
  let x = (v >= 1: int) in 
  let y = (v >= 1: int) in
  (v >= 2: int)

let baz2 x y = x + y;;

baz2 1 0;;
(* fail *)