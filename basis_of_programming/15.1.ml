(* 15.1 *)
(*
 値が重複している場合に正しく動かない。
 take_equal n lst = take n lst (=) のような関数を作り、
 [first] を (take_equal first rest) に置き換える
*)

(* 15.2 *)
(* 目的: ユークリッドの互除法に従って、ふたつの自然数の最大公約数を求める *)
(* gcd : int -> int -> int *)
let rec gcd m n = if n = 0 then m else gcd n (m mod n)

(* テスト コピペ *)
let test1 = gcd 7 5 = 1

let test2 = gcd 30 18 = 6

let test3 = gcd 36 24 = 12

(* 15.3-1 *)
(* 目的: エラトステネスのふるい。2以上n以下の自然数のリストから素数だけのリストを返す *)
(* sieve : int list -> int list *)
let rec sieve lst =
  match lst with
  | [] -> []
  | first :: rest ->
      first :: sieve (List.filter (fun n -> n mod first <> 0) rest)

(* テスト コピペ *)
let test1 = sieve [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ] = [ 2; 3; 5; 7 ]

(* 15.3-2 *)

(* 目的: 2以上n以下の自然数のリストを返す *)
(* two_to_n : int -> int list *)
let two_to_n n =
  let rec loop m = if m > n then [] else m :: loop (m + 1) in
  loop 2

(* テスト *)
let test1 = two_to_n 3 = [ 2; 3 ]

let test2 = two_to_n 10 = [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ]

(* 目的: 自然数を受け取って、それ以下の素数のリストを返す *)
(* prime : int -> int list *)
let prime n = sieve (two_to_n n)

(* テスト *)
let test1 = prime 10 = [ 2; 3; 5; 7 ]
