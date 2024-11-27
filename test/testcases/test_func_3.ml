(* cond1 implies cond2. no, yes *)
let[@rty] baz = 
  let x = (v > 3: int) in (v > 1: int);;

let baz x = x + 10;;

baz 1
(* fail *)