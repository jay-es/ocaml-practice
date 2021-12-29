(* 4.6 *)

(* 目的: 鶴の数から足の本数を計算する *)
(* tsuru_no_ashi : int -> int *)
let tsuru_no_ashi x = x * 2

(* テスト *)
let test1 = tsuru_no_ashi 1 = 2

let test2 = tsuru_no_ashi 5 = 10

let test3 = tsuru_no_ashi 10 = 20

(* 目的: 亀の数から足の本数を計算する *)
(* kame_no_ashi : int -> int *)
let kame_no_ashi x = x * 4

(* テスト *)
let test1 = kame_no_ashi 1 = 4

let test2 = kame_no_ashi 5 = 20

let test3 = kame_no_ashi 10 = 40

(* 4.7 *)

(* 目的: 鶴と亀の数から足の本数を計算する *)
(* tsurukame_no_ashi : int -> int -> int *)
let tsurukame_no_ashi x y = tsuru_no_ashi x + kame_no_ashi y

(* テスト *)
let test1 = tsurukame_no_ashi 1 1 = 6

let test2 = tsurukame_no_ashi 2 1 = 8

let test3 = tsurukame_no_ashi 1 2 = 10

(* 4.8 *)

(* 目的: 鶴と亀の合計と足の数の合計から、鶴の数を返す *)
(* tsurukame : int -> int -> int *)
let tsurukame a l = (2 * a) - (l / 2)
(*
x+y=a; -- 1
2x+4y=l; --2

2x=4a-l; 1*4 - 2
x=2a-1/2l
*)

(* テスト *)
let test1 = tsurukame 100 274 = 63

let test2 = tsurukame 17 46 = 11

let test3 = tsurukame 20 64 = 8
