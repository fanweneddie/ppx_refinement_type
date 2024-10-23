(*open Stdlib*)

let[@rty] foo = (v > 0 : int);;

let[@rty] bar = (v > 3 : int);;

let foo = 51+50+200;;

let bar = 52 + (-48);;

foo + bar
