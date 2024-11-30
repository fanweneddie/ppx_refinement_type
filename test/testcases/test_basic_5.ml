(* four and natural numbers *)

let[@rty] zero = (v = 0 : int);;

let zero = 0;;

let[@rty] four = 
  (v >= 0 : int);;

let four = zero+4;;
four;;
(* pass *)
