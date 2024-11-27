(* cond1 doesn't intersect with cond2. yes (i.e., cond1 is satisfied), yes (i.e., cond2 is satisfied) *)
let[@rty] baz = 
  let _x = (v < 0: int) in (v > 10: int);;

let baz _x = 11;;

baz (-1)
(* pass *)