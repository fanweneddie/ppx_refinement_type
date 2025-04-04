(*open Stdlib*)

(* let[@rty] p = (v >= 1: int);;

let[@rty] foo = (v > 0: int);;

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

module Example = struct
  let[@rty] foo = (v > 1 : int);;
  let[@rty] func1 =
    let x = (v >= 0: int) in (v >= 1: int);;

  let foo = 2;;
  let func2 x = let y = x in y + 1;;
  let func1 x = func2 x;;
end

let p = Example.func1 Example.foo;;

module PosEven = struct
  type _t = int;;

  let[@axiom] pos_even (n: _t): bool = 
    (n > 0) && (n mod 2 = 0)
end;;

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


let [@rty] func1 =
  let x = (v >= 0: int) in (v >= 1: int);;

let func2 x = let y = x in y + 1;;

let func1 x = func2 x;;

func1 (4+p) *)

module IList = struct
  type t = int list;;
  
  let emp (l: t): bool = 
    match l with
    | [] -> true
    | _ -> false;;

  let hd (l: t) (x: int): bool =
    match l with
    | [] -> false
    | y::_ -> x = y;;
 
  let _func (l: t): bool = (emp l || hd l 4);;

  let[@axiom] list_emp_no_hd (l : t) (x : int) = (emp l)#==>(not (hd l x)) 
end
