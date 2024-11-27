(* cond1 intersects with cond2. no, yes *)
let[@rty] baz = 
  let x = (v < 10: int) in (v >= 0: int);;

let baz x = 0;;

baz 11
(* fail *)