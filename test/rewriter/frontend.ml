(*open Stdlib*)

let[@rty] foo = (v > 0 : int);;

let[@rty] bar = (v > 3 : int);;

let[@rty] baz = 
  let x = (v >= 1: int) in 
  let y = (v >= 1: int) in
  (v >= 2: int)

let foo = (let x = 40 in x);;

let bar = 52 + (-48);;

let baz x y = x + y;;

baz (1+foo);;

baz (foo + bar);;

let[@rty] bar1 = (v > 0 : int);;

let bar1 = if true then 1 else 0;;

bar1;;


let[@rty] bar2 = 
  let x = (true: int) in (v > 0: int);;

let bar2 x = if x > 0 then x else 1 - x;;

bar2 2;;

let[@rty] rec fact =
  let n = (v >= 0: int) in (v >= 1: int);;
  
let rec fact n = 
  if n = 0 then
    1
  else
    n * fact(n - 1);;

fact 5;;

let [@rty] res = (v >= 1: int);;
let res = fact 5;;
res;;

(*let rec fact n = n * fact (n-1);;

let [@rty] func1 =
  let x = (v >= 0: int) in (v >= 1: int);;*)

(* let func2 x = let y = x in y + 1;;*)

(* let func1 x = fact x;;

func1 4 *)
