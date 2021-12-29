(* 目的: 時間を受け取って午前か午後かを返す *)
(* jikan : int -> string *)
let jikan h = if h < 12 then "am" else "pm"

(* テスト *)
let test1 = jikan 2 = "am"

let test2 = jikan 12 = "pm"

let test3 = jikan 23 = "pm"

(* 5.3 *)

(* 目的: 誕生月と日から星座を返す *)
(* seiza : int -> int -> string *)
let seiza m d = ""
(* 実装は割愛 *)

(* 5.4 *)

(* 目的: 判別式の値を返す *)
(* hanbetsushiki : float -> float -> float -> float *)
let hanbetsushiki a b c = (b ** 2.) -. (4. *. a *. c)

(* テスト *)
let test = hanbetsushiki 3. 6. 2. = 12.

(* 5.5 *)

(* 目的: 解の個数を返す *)
(* kai_no_kosuu : float -> float -> float -> int *)
let kai_no_kosuu a b c =
  if hanbetsushiki a b c > 0. then 2
  else if hanbetsushiki a b c = 0. then 1
  else 0

(* テスト *)
let test1 = kai_no_kosuu 1. (-5.) (-2.) = 2

let test2 = kai_no_kosuu 3. 2. 4. = 0
