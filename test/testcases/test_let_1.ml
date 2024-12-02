(* simple test pass *)
let[@rty] foo = (v > 0 : int);;

let foo = (let x = 40 in x);;

foo
(* pass *)
