(* cond1 implies cond2. yes, no *)
let[@rty] baz = 
  let x = (v > 3: int) in (v > 1: int);;

let baz x = x + (-3);;

baz 5
(* fail *)