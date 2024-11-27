(* cond1 implies cond2. no, no *)
let[@rty] baz = 
  let x = (v > 3: int) in (v > 1: int);;

let baz x = x + (-1);;

baz 1
(* fail *)