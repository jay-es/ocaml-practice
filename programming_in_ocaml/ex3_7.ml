(* 3.7 *)
(* x^n を計算する関数 *)
(* ①再帰呼び出しを n 回伴う定義 *)
(* float*int -> float *)
let rec pow (x, n) = if n = 0 then 1. else x *. pow (x, n - 1)

let test1 = pow (2., 3) = 8.

(* 3.8 *)
(* 指数関数を反復的に定義 *)
(* float*int -> float *)
let iterpow (x, n) =
  let rec loop (i, res) = if i = n then res else loop (i + 1, res *. x) in
  loop (0, 1.)

let test1 = iterpow (2., 3) = 8.

(* 3.13 *)
(* pow をカリー化 *)
(* int -> float -> float *)
let rec pow n x = if n = 0 then 1. else x *. pow (n - 1) x

let test1 = pow 3 2. = 8.

(* 部分適用で3乗する関数 cube を作る *)
(* float -> float *)
let cube = pow 3

let test1 = cube 2. = 8.

(* 引数の順番が反対の場合、どのように cube を作れるか *)
let rec pow x n = if n = 0 then 1. else x *. pow x (n - 1)

let cube x = pow x 3

let test1 = cube 2. = 8.
