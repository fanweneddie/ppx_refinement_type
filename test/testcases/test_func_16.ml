(* cond1 doesn't intersect with cond2. no, no *)
let[@rty] baz = 
  let x = (v < 0: int) in (v > 10: int);;

let baz x = x + 100;;

baz 0
(* fail *)