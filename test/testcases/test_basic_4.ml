(* zero and natural numbers *)
let[@rty] zero = (v = 0 : int);;

let zero = 0;;

zero;;

let[@rty] zeronat = 
  (v >= 0 : int);;

let zeronat = zero ;;
zeronat;;
(* pass *)