(* 10.5 *)

type gakusei_t = { namae : string; tensuu : int }

let alice = { namae = "Alice"; tensuu = 90 }

let bob = { namae = "Bob"; tensuu = 80 }

let charlie = { namae = "Charlie"; tensuu = 70 }

(* 目的: 一番点数の高い学生を返す *)
(* gakusei_max : gakusei_t list -> gakusei_t *)

let rec gakusei_max lst =
  match lst with
  | [] -> { namae = ""; tensuu = min_int }
  | ({ tensuu = t1 } as first) :: rest -> (
      match gakusei_max rest with
      | { tensuu = t2 } -> if t1 > t2 then first else gakusei_max rest)

(* テスト *)
let test1 = gakusei_max [ alice; bob ] = alice

let test2 = gakusei_max [ charlie; bob ] = bob

let test3 = gakusei_max [ charlie; alice; bob ] = alice

(* 10.6 *)

(* 目的: 10.5 の gakusei_max を局所変数定義で書き直し *)
let rec gakusei_max lst =
  match lst with
  | [] -> { namae = ""; tensuu = min_int }
  | ({ tensuu = t1 } as first) :: rest -> (
      let max = gakusei_max rest in
      match max with { tensuu = t2 } -> if t1 > t2 then first else max)

(* テスト *)
let test1 = gakusei_max [ alice; bob ] = alice

let test2 = gakusei_max [ charlie; bob ] = bob

let test3 = gakusei_max [ charlie; alice; bob ] = alice

(* 10.7 *)
type person_t = { blood_type : string }

let person1 = { blood_type = "A" }

let person2 = { blood_type = "B" }

let person3 = { blood_type = "O" }

let person4 = { blood_type = "AB" }

let person5 = { blood_type = "O" }

(* 目的: 各血液型の人数を集計する *)
(* ketsueki_shukei : person_t list -> int*int*int*int *)
let rec ketsueki_shukei lst =
  match lst with
  | [] -> (0, 0, 0, 0)
  | { blood_type = bt } :: rest -> (
      let a, b, o, ab = ketsueki_shukei rest in
      match bt with
      | "A" -> (a + 1, b, o, ab)
      | "B" -> (a, b + 1, o, ab)
      | "O" -> (a, b, o + 1, ab)
      | "AB" -> (a, b, o, ab + 1)
      | _ -> (a, b, o, ab))

let test1 = ketsueki_shukei [] = (0, 0, 0, 0)

let test2 = ketsueki_shukei [ person1 ] = (1, 0, 0, 0)

let test3 = ketsueki_shukei [ person1; person3 ] = (1, 0, 1, 0)

let test4 =
  ketsueki_shukei [ person1; person3; person2; person4; person5 ] = (1, 1, 2, 1)

(* 10.8 *)

let rec max_one lst =
  match lst with
  | [] -> min_int
  | first :: rest ->
      let rest_max = max_one rest in
      if first > rest_max then first else rest_max

let test1 = max_one [ 1 ] = 1

let test2 = max_one [ 1; 2 ] = 2

let test3 = max_one [ 2; 1 ] = 2

let test4 = max_one [ 5; 3; 8; 1; 7; 4 ] = 8

(* 目的: 最も人数の多い血液型を返す *)
(* saita_ketsueki : person_t list -> string *)
let rec saita_ketsueki lst =
  let a, b, o, ab = ketsueki_shukei lst in
  let max_num = max_one [ a; b; o; ab ] in
  if max_num = a then "A"
  else if max_num = b then "B"
  else if max_num = o then "O"
  else "AB"

let test1 = saita_ketsueki [ person1 ] = "A"

let test2 = saita_ketsueki [ person1; person3; person2; person4; person5 ] = "O"

(* 10.9 *)
(* 目的: ふたつのリストの長さが同じかどうかを判定する *)
(* equal_length : 'a list -> 'a list -> bool *)
let rec equal_length lst1 lst2 =
  match (lst1, lst2) with
  | [], [] -> true
  | first1 :: rest1, [] -> false
  | [], first2 :: rest2 -> false
  | first1 :: rest1, first2 :: rest2 -> equal_length rest1 rest2

(* テスト *)
let test1 = equal_length [] [] = true

let test2 = equal_length [] [ 1 ] = false

let test3 = equal_length [ 2 ] [ 1 ] = true

let test4 = equal_length [ 2; 3 ] [ 1 ] = false
