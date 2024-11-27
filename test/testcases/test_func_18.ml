(* argument name is v, i.e., the same as the variable name in the refinement constraint *)
let[@rty] baz = 
  let v = (v > 1: int) in (v > 3: int);;

let baz v = v + 2;;

baz 2
(* pass *)