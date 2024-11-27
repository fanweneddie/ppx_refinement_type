(* cond1 intersects with cond2. no, no *)
let[@rty] baz = 
  let x = (v < 10: int) in (v >= 0: int);;

let baz x = x + 1;;

baz 11
(* fail *)