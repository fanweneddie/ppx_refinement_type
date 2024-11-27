(* multiple arguments, not satisfying the constraint *)
let[@rty] baz1 = 
  let x = (v >= 1: int) in 
  let y = (v >= 1: int) in
  (v >= 2: int)

let baz1 x y = x + y;;

baz1 0 1;;
(* fail *)