(* cond1 implies cond2. yes (i.e., cond1 is satisfied), yes (i.e., cond2 is satisfied) *)
let[@rty] baz = 
  let x = (v > 3: int) in (v > 1: int);;

let baz x = x + 1;;

baz 5
(* pass *)