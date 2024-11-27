(* different name of parameter and argument *)
let[@rty] baz = 
  let x = (v > 1: int) in (v > 3: int);;

let baz y = y + 2;;

baz 2
(* fail *)