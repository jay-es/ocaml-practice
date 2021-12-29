(* 目的: 階乗を返す *)
(* fac : int -> int *)
let rec fac n = if n = 0 then 1 else n * fac (n - 1)

let test1 = fac 0 = 1

let test2 = fac 1 = 1

let test3 = fac 2 = 2

let test4 = fac 3 = 6

let test5 = fac 4 = 24

let test6 = fac 10 = 3628800

(* 11.1 *)

let square n = n * n

(* 目的: 0 から受け取った自然数までの2乗の和を求める *)
(* sum_of_square : int -> int *)
let rec sum_of_square n = if n = 0 then 0 else square n + sum_of_square (n - 1)

(* テスト *)
let test1 = sum_of_square 0 = 0

let test2 = sum_of_square 1 = 1

let test3 = sum_of_square 2 = 5

let test4 = sum_of_square 3 = 14

let test5 = sum_of_square 4 = 30

(* 11.2 *)

(* 目的: 111ページにある漸化式の第n項を求める *)
(* a : int -> int *)
let rec a n = if n = 0 then 3 else (2 * a (n - 1)) - 1

(* テスト（回答例ファイルからコピペ） *)
let test1 = a 0 = 3

let test2 = a 1 = 5

let test3 = a 2 = 9

let test4 = a 3 = 17

let test5 = a 4 = 33
