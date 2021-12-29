(* 3.6 *)
(* ①2実数の相乗平均をとる関数 *)
(* float*float -> float *)
let geo_mean (x, y) = sqrt (x *. y)

let test1 = geo_mean (2., 8.) = 4.

(* ②BMI *)
(* string*float*float -> string *)
let bmi (name, height, weight) =
  let v = weight /. (height ** 2.) in
  let state =
    if 18.5 > v then "やせ"
    else if 25. > v then "標準"
    else if 30. > v then "肥満"
    else "高度肥満"
  in
  name ^ "さんは" ^ state ^ "です"

let test1 = bmi ("aaa", 1.6, 60.) = "aaaさんは標準です"

let test2 = bmi ("bbb", 1.6, 70.) = "bbbさんは肥満です"

(* ③ 任意の整数 x，y に対し，f (sum_and_diff (x,y)) が (x,y) を返すような関数 f *)
let sum_and_diff (x, y) = (x + y, x - y)

(* int*int -> int*int *)
let f (m, n) = ((m + n) / 2, (m - n) / 2)

let test1 = f (sum_and_diff (1, 2)) = (1, 2)

let test2 = f (sum_and_diff (-5, 7)) = (-5, 7)
