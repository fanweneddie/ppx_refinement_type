(* cond1 is implied by cond2. yes, no *)
let[@rty] baz = 
  let x = (v < 10: int) in (v < 5: int);;

let baz x = x + (-4);;

baz 8
(* fail *)