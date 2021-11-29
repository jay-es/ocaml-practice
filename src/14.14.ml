(* 14.14 *)
(* 問題 9.6 や 14.3 の concat を 1行で書き直す *)

(* concat : string list -> string *)
let concat lst = List.fold_right ( ^ ) lst ""

(* テスト *)
let test1 = concat [] = ""

let test2 = concat [ "あ" ] = "あ"

let test3 = concat [ "春"; "夏"; "秋"; "冬" ] = "春夏秋冬"

(* 14.15 *)
let rec enumerate n = if n = 0 then [] else n :: enumerate (n - 1)

(* 目的: 1から受け取った自然数までの合計を求める *)
(* one_to_n : int -> int *)
let one_to_n n = List.fold_right ( + ) (enumerate n) 0

(* テスト コピペ*)
let test1 = one_to_n 0 = 0

let test2 = one_to_n 1 = 1

let test3 = one_to_n 2 = 3

let test4 = one_to_n 10 = 55

(* 14.16 *)
(* 目的: 階乗を求める *)
let fac n = List.fold_right ( * ) (enumerate n) 1

(* テスト コピペ*)
let test1 = fac 0 = 1

let test2 = fac 1 = 1

let test3 = fac 2 = 2

let test4 = fac 3 = 6

let test5 = fac 4 = 24

let test6 = fac 10 = 3628800
