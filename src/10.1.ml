(* 10.1 *)

(* 目的: 昇順の整数リストの間に数字を入れる *)
(* insert : int list -> int -> int list *)
let rec insert lst n =
  match lst with
  | [] -> [ n ]
  | first :: rest -> if first < n then first :: insert rest n else n :: lst

(* テスト *)
let test1 = insert [] 1 = [ 1 ]

let test2 = insert [ 1 ] 2 = [ 1; 2 ]

let test3 = insert [ 2 ] 1 = [ 1; 2 ]

let test4 = insert [ 1; 2; 4; 5 ] 3 = [ 1; 2; 3; 4; 5 ]

(* 10.2 *)

(* 目的: 整数のリストを昇順に整列 *)
(* ins_sort : int list -> int list *)
let rec ins_sort lst =
  match lst with [] -> [] | first :: rest -> insert (ins_sort rest) first

let test1 = ins_sort [] = []

let test2 = ins_sort [ 2; 1 ] = [ 1; 2 ]

let test3 = ins_sort [ 5; 3; 8; 1; 7; 4 ] = [ 1; 3; 4; 5; 7; 8 ]

(* 10.3 *)

type gakusei_t = { namae : string; tensuu : int }

let alice = { namae = "Alice"; tensuu = 90 }

let bob = { namae = "Bob"; tensuu = 80 }

let charlie = { namae = "Charlie"; tensuu = 70 }

let rec gakusei_insert lst gakusei =
  match lst with
  | [] -> [ gakusei ]
  | ({ tensuu = t1 } as first) :: rest -> (
      match gakusei with
      | { tensuu = t2 } ->
          if t1 < t2 then first :: gakusei_insert rest gakusei
          else gakusei :: lst)

(* 目的: 点数順にソート *)
(* gakusei_sort : gakusei_t list -> gakusei_t list *)
let rec gakusei_sort lst =
  match lst with
  | [] -> []
  | first :: rest -> gakusei_insert (gakusei_sort rest) first

(* テスト *)
let test1 = gakusei_sort [] = []

let test2 = gakusei_sort [ alice; charlie ] = [ charlie; alice ]

let test3 = gakusei_sort [ alice; charlie; bob ] = [ charlie; bob; alice ]

(* 10.4 *)

let rec person_insert lst person =
  match lst with
  | [] -> [ person ]
  | first :: rest ->
      if first.namae < person.namae then first :: insert rest person
      else person :: lst

(* 目的: 名前順にソート（本当の課題は person_t を並べ替える） *)
(* person_sort : person_t list -> person_t list *)
let rec person_sort lst =
  match lst with
  | [] -> []
  | first :: rest -> person_insert (person_sort rest) first

(* テスト *)
let test1 = person_sort [] = []

let test2 = person_sort [ charlie; alice ] = [ alice; charlie ]

let test3 = person_sort [ bob; alice; charlie ] = [ alice; bob; charlie ]
