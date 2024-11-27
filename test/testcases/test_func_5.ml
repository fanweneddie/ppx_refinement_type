(* cond1 is implied by cond2. yes (i.e., cond1 is satisfied), yes (i.e., cond2 is satisfied) *)
let[@rty] baz = 
  let x = (v < 10: int) in (v < 5: int);;

let baz x = x + (-5);;

baz 8
(* pass *)