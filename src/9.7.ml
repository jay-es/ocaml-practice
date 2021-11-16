(* 9.7 *)

type person_t = {
  name : string;
  (* height : float; *)
  (* weight : float; *)
  (* birthday : int * int; *)
  blood_type : string;
}

let lst1 = []

let lst2 = [ { name = "Alice"; blood_type = "A" } ]

let lst3 =
  [ { name = "Alice"; blood_type = "A" }; { name = "Bob"; blood_type = "B" } ]

let lst4 =
  [
    { name = "Alice"; blood_type = "A" };
    { name = "Bob"; blood_type = "B" };
    { name = "Charlie"; blood_type = "A" };
  ]

(* 目的: 血液型がAの人の数を返す *)
(* count_ketsueki_A : person_t list -> int *)
let rec count_ketsueki_A lst =
  match lst with
  | [] -> 0
  | { name = n; blood_type = b } :: rest ->
      if b = "A" then 1 + count_ketsueki_A rest else count_ketsueki_A rest

(* テスト *)
let test1 = count_ketsueki_A lst1 = 0

let test2 = count_ketsueki_A lst2 = 1

let test3 = count_ketsueki_A lst3 = 1

let test4 = count_ketsueki_A lst4 = 2

(* 9.8 *)

(* 目的: 血液型がAの人の名前のリストを返す（本当の課題は乙女座の人の名前） *)
(* ketsueki_A : person_t list -> string list *)
let rec ketsueki_A lst =
  match lst with
  | [] -> []
  | { name = n; blood_type = b } :: rest ->
      if b = "A" then n :: ketsueki_A rest else ketsueki_A rest

(* テスト *)
let test1 = ketsueki_A lst1 = []

let test2 = ketsueki_A lst2 = [ "Alice" ]

let test3 = ketsueki_A lst3 = [ "Alice" ]

let test4 = ketsueki_A lst4 = [ "Alice"; "Charlie" ]
