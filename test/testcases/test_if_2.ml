let[@rty] bar2 = 
  let x = (true: int) in (v > 0: int);;

let bar2 x = if x > 0 then x else 1 - x;;

bar2 2;;
(* pass *)
