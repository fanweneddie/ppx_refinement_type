(* testing recursive functional call *)
let[@rty] rec fact =
  let n = (v >= 0: int) in (v >= 1: int);;
  
let rec fact n = 
  if n = 0 then
    1
  else
    n * fact(n - 1);;

fact(5);;

let [@rty] res = (v >= 1: int);;
let res = fact(5);;
res;;

(* pass *)