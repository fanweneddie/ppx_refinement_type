(*open Stdlib*)

(*let[@rty] foo = (v > 0 : int);;

let[@rty] bar = (v > 3 : int);;*)

let[@rty] baz = 
  let x = (v > 2: int) in (v > 3: int)

(*let foo = 51+50+200;;

let bar = 52 + (-48);;*)

let baz x = x + 2;;

(* baz (foo + bar) *)
