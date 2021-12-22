(* 22.1 *)
(* 目的: 文字列を受け取り、関数が呼ばれた回数を付加したものを返す *)

let count = ref (-1)

(* gensym : string -> string *)
let gensym str =
  count := !count + 1;
  str ^ string_of_int !count

(* テスト *)
let test1 = gensym "a" = "a0"

let test2 = gensym "a" = "a1"

let test3 = gensym "x" = "x2"

(* 22.2 *)
(* 目的: 配列を与えたら、フィボナッチ数列を入れた配列を返す *)
let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

(* fib_array : int array -> int array *)
let fib_array arr =
  Array.iteri (fun i n -> arr.(i) <- fib i) arr;
  arr

(* fib 使わないバージョン *)
let fib_array arr =
  Array.iteri
    (fun i n -> arr.(i) <- (if i < 2 then i else arr.(i - 1) + arr.(i - 2)))
    arr;
  arr

(* テスト *)
let test1 = fib_array [||] = [||]

let test2 = fib_array [| 1; 1 |] = [| 0; 1 |]

let test1 = fib_array [| 5; 5; 5; 5; 5; 5; 5 |] = [| 0; 1; 1; 2; 3; 5; 8 |]
