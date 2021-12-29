(* 7.1 *)

(* 目的: 5強化の点数から合計点と平均点を組にして返す *)
(* goukei_to_heikin : int -> int -> int -> int -> int -> int * int *)
let goukei_to_heikin j m e sc ss =
  (j + m + e + sc + ss, (j + m + e + sc + ss) / 5)

(* テスト *)
let test = goukei_to_heikin 10 10 10 10 10 = (50, 10)

(* 7.2 *)

(* 目的: 名前と成績の組から文字列を返す *)
(* seiseki : (string * string) -> string *)
let seiseki (name, rank) = name ^ "さんの評価は" ^ rank ^ "です"

(* テスト *)
let test1 = seiseki ("John", "A") = "Johnさんの評価はAです"

let test2 = seiseki ("Doe", "C") = "Doeさんの評価はCです"

(* 7.3 *)

(* 目的: 平面座標を受け取って、x軸に対して対象な点の座標を返す *)
(* taisho_x : int * int -> int * int *)
(* let taisho_x (x, y) = (-x, y) *)
let taisho_x point = match point with x, y -> (-x, y)

(* テスト *)
let test1 = taisho_x (1, 2) = (-1, 2)

let test2 = taisho_x (-3, -4) = (3, -4)

(* 7.4 *)

(* 目的: 平面座標を2つ受け取り、中点を求める *)
(* chuten : float*float -> float*float -> float*float *)
(* let chuten (x1, y1) (x2, y2) = ((x1 +. x2) /. 2., (y1 +. y2) /. 2.) *)
let chuten p1 p2 =
  match p1 with
  | x1, y1 -> ( match p2 with x2, y2 -> ((x1 +. x2) /. 2., (y1 +. y2) /. 2.))

(* テスト *)
let test1 = chuten (2., 3.) (4., 1.) = (3., 2.)

let test2 = chuten (-1., 3.) (1., -1.) = (0., 1.)
