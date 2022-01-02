(* 8.1 *)
(* ref, !，:= を再定義 *)
let ref v = { contents = v }

let ( ! ) = function { contents } -> contents

let ( := ) r v = r.contents <- v

let a_ref = ref 0

let test1 = !a_ref = 0;;

a_ref := 1

let test2 = !a_ref = 1

(* 8.2 *)
(* 参照の指す先の整数を1 増やす関数incr *)
let incr r = r := !r + 1

let x = ref 3;;

incr x

let test1 = !x = 4

(* 8.4 *)
(* 参照と繰り返しの構文（while，for）を使ってフィボナッチ数を求める関数 *)
(* int -> int *)
let fib n =
  let ans = ref 0 in
  let next = ref 1 in
  let cnt = ref 0 in
  while !cnt < n do
    let sum = !ans + !next in
    ans := !next;
    next := sum;
    cnt := !cnt + 1
  done;
  !ans

let test1 =
  [ fib 1; fib 2; fib 3; fib 4; fib 5; fib 6; fib 7; fib 8; fib 9; fib 10 ]
  = [ 1; 1; 2; 3; 5; 8; 13; 21; 34; 55 ]

(* int -> int *)
let fib n =
  let ans = ref 0 in
  let next = ref 1 in
  for i = 0 to n - 1 do
    let sum = !ans + !next in
    ans := !next;
    next := sum
  done;
  !ans

let test1 =
  [ fib 1; fib 2; fib 3; fib 4; fib 5; fib 6; fib 7; fib 8; fib 9; fib 10 ]
  = [ 1; 1; 2; 3; 5; 8; 13; 21; 34; 55 ]

(* 8.10 *)
(* print_int 関数をstdout，output_string などを用いて定義 *)
(* int -> unit *)
let print_int n = output_string stdout (string_of_int n)

let test1 = print_int 42 = ()

(* 8.11 *)
(* ファイル名を引数にとって，そのファイルの内容を行番号付きで表示させる関数display_file *)
(* string -> unit *)
let display_file filename =
  let ch = open_in filename in
  let no = ref 1 in
  try
    while true do
      print_endline (string_of_int !no ^ "\t" ^ input_line ch);
      incr no
    done
  with End_of_file -> close_in ch

let test1 =
  try
    display_file "";
    false
  with Sys_error _ -> true

let test2 = display_file "../README.md" = ()
