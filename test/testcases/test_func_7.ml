(* cond1 is implied by cond2. no, yes *)
let[@rty] baz = 
  let x = (v < 10: int) in (v < 5: int);;

let baz x = x + (-100);;

baz 11
(* fail *)