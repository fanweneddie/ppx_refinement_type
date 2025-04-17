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

  let rec equal (l1: t) (l2: t): bool =
    match l1, l2 with
    | [], [] -> true
    | hd1::tl1, hd2::tl2 -> (hd1 = hd2) && (equal tl1 tl2)
    | _, _ -> false;;

  let rec tl (l: t) (l1: t): bool =
    match l with
    | [] -> equal l l1
    | _::t -> (equal l l1) || (tl t l1);;

  let cons (x: int) (l: t): t = x::l;;

  let _func (l: t): bool = (emp l || hd l 4 || tl (cons 4 l) []);;

  let[@axiom] list_emp_no_hd (l : t) (x : int) = (emp l)#==>(not (hd l x));;

  let[@axiom] list_emp_no_tl (l : t) (l1 : t) =
    (emp l)#==>(not (tl l l1));;
  
  let[@axiom] list_no_emp_exists_tl (l : t) ((l1 [@exists]) : t) =
    (not (emp l))#==>(tl l l1);;

  let[@axiom] list_cons (l: t) (x: int) = hd (cons x l) x;;
end

module IStack = struct
  type t = IList.t;;

  let[@rty] push =
    let l = (true: t) in 
    let x = (true: int) in
    (true: t)

  let push (l: t) (x: int): t = List.cons x l;;

  let pop (l: t): t =
    match l with
    | [] -> []
    | _::tl -> tl
end;;

IStack.push [] 4 |> IStack.pop
