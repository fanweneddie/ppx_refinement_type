(* cond1 intersects with cond2. yes (i.e., cond1 is satisfied), yes (i.e., cond2 is satisfied) *)
let[@rty] baz = 
  let _x = (v < 10: int) in (v >= 0: int);;

let baz _x = 2;;

baz 1
(* pass *)