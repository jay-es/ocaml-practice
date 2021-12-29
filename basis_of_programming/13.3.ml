(* 13.3 *)
(* 'a -> 'a *)
let fn x = x

(* 'a -> 'b -> 'a *)
let fn x y = x

(* 'a -> 'b -> 'b *)
let fn x y = y

(* 'a -> ('a -> 'b) -> 'b *)
let fn x y = y x

(* ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
let fn x y z = y (x z)

(* 13.4 *)
(* 関数2つを合成した関数を返す *)
let compose fn1 fn2 =
  let f x = fn1 (fn2 x) in
  f

let time2 x = x * 2

let add3 x = x + 3

let test1 = (compose time2 add3) 4 = 14

(* 13.5 *)
let twice f =
  let g x = f (f x) in
  g

let test1 = twice twice
