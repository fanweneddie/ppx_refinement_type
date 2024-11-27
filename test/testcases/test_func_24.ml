(* partial application *)
let[@rty] baz = 
  let x = (v >= 1: int) in 
  let y = (v >= 1: int) in
  (v >= 2: int)

let baz x y = x + y;;

baz 1;;
(* pass *)