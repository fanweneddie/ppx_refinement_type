(* simple test fail*)
let[@rty] foo = (v > 0 : int);;

let foo = (let x = -1 in x);;

foo
(* fail *)
