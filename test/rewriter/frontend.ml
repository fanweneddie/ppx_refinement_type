(*open Stdlib*)

let[@rty] foo = (v > 0 : int);;

(*let[@rty] bar = (v > 3 : int);;*)

let[@rty] baz = 
  let x = (v > 1: int) in (v > 3: int)

let foo = (let x = 40 in x);;

(*let bar = 52 + (-48);;*)

let baz x = x + 2;;

baz (1+foo)

(* baz (foo + bar) *)
