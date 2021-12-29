(* 14.5-1 *)
(* 14.1 の関数を局所定義で書き直す *)
(* even : int list -> int list *)
let even lst =
  let is_even n = n mod 2 = 0 in
  List.filter is_even lst

(* テスト *)
let test1 = even [] = []

let test2 = even [ 1 ] = []

let test3 = even [ 2 ] = [ 2 ]

let test4 = even [ 1; 2; 3; 4; 5 ] = [ 2; 4 ]

(* 14.5-2 *)
(* 14.2 の関数を局所定義で書き直す *)
type gakusei_t = { namae : string; seiseki : string }

let alice = { namae = "Alice"; seiseki = "A" }

let bob = { namae = "Bob"; seiseki = "B" }

let charlie = { namae = "Charlie"; seiseki = "A" }

(* count_A : gakusei_t list -> int *)
let count_A lst =
  let is_A gakusei =
    match gakusei with { namae = n; seiseki = s } -> s = "A"
  in
  List.length (List.filter is_A lst)

(* テスト *)
let test1 = count_A [] = 0

let test2 = count_A [ alice; bob ] = 1

let test2 = count_A [ alice; bob; charlie ] = 2

(* 14.5-3 *)
(* 14.3 の関数を局所定義で書き直す *)
(* concat : string list -> string *)
let concat lst =
  let f a b = a ^ b in
  List.fold_right f lst ""

(* テスト *)
let test1 = concat [] = ""

let test2 = concat [ "あ" ] = "あ"

let test3 = concat [ "春"; "夏"; "秋"; "冬" ] = "春夏秋冬"

(* 14.5-4 *)
(* 14.4 の関数を局所定義で書き直す *)
type gakusei_t = { namae : string; tensuu : int }

let alice = { namae = "Alice"; tensuu = 90 }

let bob = { namae = "Bob"; tensuu = 80 }

(* gakusei_sum : gakusei_t list -> int *)
let gakusei_sum lst =
  let add gakusei rest_tensuu =
    match gakusei with { namae = n; tensuu = t } -> t + rest_tensuu
  in
  List.fold_right add lst 0

(* テスト *)
let test1 = gakusei_sum [] = 0

let test2 = gakusei_sum [ alice ] = 90

let test3 = gakusei_sum [ alice; bob ] = 170
