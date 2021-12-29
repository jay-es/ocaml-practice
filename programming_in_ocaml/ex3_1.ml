(* 3.1 *)

let round n = truncate (n +. 0.5)

let test1 = round 0. = 0

let test2 = round 0.499 = 0

let test3 = round 0.50 = 1

let round_cent n = floor ((n *. 100.) +. 0.5) /. 100.

let test1 = round_cent 0.0 = 0.

let test2 = round_cent 0.00499 = 0.

let test3 = round_cent 0.0050 = 0.01

(* ①USドルを受け取って円に換算する関数 *)
(* float -> int *)
let yen_of_dollar d = round (d *. 114.32)

let test1 = yen_of_dollar 1.0 = 114

(* ②円を受け取ってUSドルに換算する関数 *)
(* int -> float *)
let doller_of_yen y = round_cent (float_of_int y /. 114.32)

let test1 = doller_of_yen 114 = 1.

(* ③USドルを受け取って文字列を返す *)
(* float -> string *)
let disp d =
  let y = yen_of_dollar d in
  string_of_float d ^ " dollars are " ^ string_of_int y ^ " yen."

let test1 = disp 1.0 = "1. dollars are 114 yen."

(* ④アルファベットの小文字なら大文字にする関数 *)
(* char -> char *)
let capitalize c =
  let n = int_of_char c in
  if 97 <= n && n <= 122 then char_of_int (n - 32) else c

let test1 = capitalize 'a' = 'A'

let test2 = capitalize '1' = '1'

(* 3.2 *)
(* b1 && b2 を、if式,true,false,b1,b2 のみを用いて書き直す。 b1 || b2 も *)
let and_ b1 b2 = (b1 && b2) = if b1 then b2 else b1

let test1 = (and_ true true, and_ true false, and_ false true, and_ false false)

let or_ b1 b2 = (b1 || b2) = if b1 then b1 else b2

let test1 = (or_ true true, or_ true false, or_ false true, or_ false false)

(* 3.3 *)
(* b1 && b2 を、||,not,b1,b2 のみを用いて書き直す *)
let and_ b1 b2 = (b1 && b2) = not ((not b1) || not b2)

let test1 = (and_ true true, and_ true false, and_ false true, and_ false false)

(* b1 || b2 を、&&,not,b1,b2 のみを用いて書き直す *)
let or_ b1 b2 = (b1 || b2) = not ((not b1) && not b2)

let test1 = (or_ true true, or_ true false, or_ false true, or_ false false)

(* 3.4 *)
(* それぞれの変数の参照がどの宣言を指すか。また式の評価結果を予想→実行して確認 *)
(* 25, 15, 16 → 25, 13, 16 が正しい*)
