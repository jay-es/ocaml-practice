(* 4.1 *)
(* カリー化の反対の関数。組を受け取るように変換する *)
(* (’a -> ’b -> ’c) -> ’a * ’b -> ’c *)
let uncurry f (x, y) = f x y

let ave x y = (x +. y) /. 2.

let test1 = (uncurry ave) (4.0, 5.3) = 4.65

(* 4.2 *)
(* repeat を使ってフィボナッチを計算する関数を作る *)
let rec repeat f n x = if n > 0 then repeat f (n - 1) (f x) else x

(* int -> int *)
let fib n =
  let fibn, _ = repeat (fun (x, y) -> (y, x + y)) n (0, 1) in
  fibn

let test1 =
  [ fib 1; fib 2; fib 3; fib 4; fib 5; fib 6; fib 7; fib 8; fib 9; fib 10 ]
  = [ 1; 1; 2; 3; 5; 8; 13; 21; 34; 55 ]
