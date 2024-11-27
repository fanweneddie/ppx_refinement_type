(* cond1 intersects with cond2. yes, no *)
let[@rty] baz = 
  let x = (v < 10: int) in (v >= 0: int);;

let baz x = x;;

baz 1
(* fail *)