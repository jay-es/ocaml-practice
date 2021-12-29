(* 13.1 *)

type person_t = { name : string; blood_type : string }

let person_list =
  [
    { name = "Alice"; blood_type = "A" };
    { name = "Bob"; blood_type = "B" };
    { name = "Charlie"; blood_type = "A" };
    (* { name = "Dave"; blood_type = "AB" }; *)
    (* { name = "Ellen"; blood_type = "O" }; *)
    (* { name = "Frank"; blood_type = "O" }; *)
  ]

(* 目的: 特定の血液型の人数を調べる *)
(* count_ketsueki : person_t list -> string -> int *)
let rec count_ketsueki lst type0 =
  match lst with
  | [] -> 0
  | { name = n; blood_type = b } :: rest ->
      (if b = type0 then 1 else 0) + count_ketsueki rest type0

(* テスト *)
let test1 = count_ketsueki [] "A" = 0

let test2 = count_ketsueki person_list "Z" = 0

let test3 = count_ketsueki person_list "A" = 2

let test4 = count_ketsueki person_list "B" = 1

(* 13.2 *)
(* 目的: 全員の名前のリストを返す *)
(* person_namae : person_t list -> string list *)
let get_namae p = match p with { name = n; blood_type = b } -> n

let person_namae lst = List.map get_namae lst

(* テスト *)
let test1 = person_namae [] = []

let test1 = person_namae person_list = [ "Alice"; "Bob"; "Charlie" ]
