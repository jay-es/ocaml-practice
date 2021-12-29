(* 9.4 *)

(* 目的: 受け取ったリストの長さを返す *)
(* length : int list -> int *)
let rec length lst = match lst with [] -> 0 | first :: rest -> 1 + length rest

(* テスト *)
let test1 = length [] = 0

let test2 = length [ 2 ] = 1

let test3 = length [ 2; 4; 6; 8 ] = 4

(* 9.5 *)

(* 目的: 受け取ったリストのうち、偶数の要素だけのリストを返す *)
(* even : int list -> int list *)
let rec even lst =
  match lst with
  | [] -> []
  | first :: rest -> if first mod 2 = 0 then first :: even rest else even rest

(* テスト *)
let test1 = even [] = []

let test2 = even [ 1 ] = []

let test3 = even [ 2 ] = [ 2 ]

let test4 = even [ 1; 2; 3; 4; 5 ] = [ 2; 4 ]

(* 9.6 *)

(* 目的: 受け取ったリストを結合して文字列返す *)
(* concat : string list -> string *)
let rec concat lst =
  match lst with [] -> "" | first :: rest -> first ^ concat rest

(* テスト *)
let test1 = concat [] = ""

let test2 = concat [ "あ" ] = "あ"

let test3 = concat [ "春"; "夏"; "秋"; "冬" ] = "春夏秋冬"
