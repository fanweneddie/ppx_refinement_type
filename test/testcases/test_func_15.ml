(* cond1 doesn't intersect with cond2. no, yes *)
let[@rty] baz = 
  let x = (v < 0: int) in (v > 10: int);;

let baz x = 11;;

baz 0
(* fail *)