(* 14.8*)
(* 目的: 整数の2乗から1引いた数を返す名前のない関数 *)
(* int -> int *)
fun n -> (n * n) - 1

(* 14.9 *)
(* 目的: person_t の 名前フィールドを取り出す、名前のない関数 *)
type person_t = { namae : string };;

fun person -> match person with { namae = n } -> n

(* 14.10-1 *)
(* 14.5-1 の関数を名前のない関数を使って書き直す *)
(* even : int list -> int list *)
let even lst = List.filter (fun n -> n mod 2 = 0) lst

(* テスト *)
let test1 = even [] = []

let test2 = even [ 1 ] = []

let test3 = even [ 2 ] = [ 2 ]

let test4 = even [ 1; 2; 3; 4; 5 ] = [ 2; 4 ]

(* 14.10-2 *)
(* 14.5-2 の関数を名前のない関数を使って書き直す *)
type gakusei_t = { namae : string; seiseki : string }

let alice = { namae = "Alice"; seiseki = "A" }

let bob = { namae = "Bob"; seiseki = "B" }

let charlie = { namae = "Charlie"; seiseki = "A" }

(* count_A : gakusei_t list -> int *)
let count_A lst =
  List.length
    (List.filter
       (fun gakusei ->
         match gakusei with { namae = n; seiseki = s } -> s = "A")
       lst)

(* テスト *)
let test1 = count_A [] = 0

let test2 = count_A [ alice; bob ] = 1

let test2 = count_A [ alice; bob; charlie ] = 2

(* 14.10-3 *)
(* 14.5-3 の関数を名前のない関数を使って書き直す *)
(* concat : string list -> string *)
let concat lst = List.fold_right (fun a b -> a ^ b) lst ""

(* テスト *)
let test1 = concat [] = ""

let test2 = concat [ "あ" ] = "あ"

let test3 = concat [ "春"; "夏"; "秋"; "冬" ] = "春夏秋冬"

(* 14.10-4 *)
(* 14.5-4 の関数を名前のない関数を使って書き直す *)
type gakusei_t = { namae : string; tensuu : int }

let alice = { namae = "Alice"; tensuu = 90 }

let bob = { namae = "Bob"; tensuu = 80 }

(* gakusei_sum : gakusei_t list -> int *)
let gakusei_sum lst =
  List.fold_right
    (fun gakusei rest ->
      match gakusei with { namae = n; tensuu = t } -> t + rest)
    lst 0

(* テスト *)
let test1 = gakusei_sum [] = 0

let test2 = gakusei_sum [ alice ] = 90

let test3 = gakusei_sum [ alice; bob ] = 170
