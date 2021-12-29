(* 16.1 *)
(* 目的: 整数のリストを受け取り、それまでの数の合計からなるリストを作成する *)
(* sum_list : int list -> int list *)
let sum_list lst =
  let rec loop lst acc =
    match lst with
    | [] -> []
    | first :: rest ->
        let total = first + acc in
        total :: loop rest total
  in
  loop lst 0

(* テスト *)
let test1 = sum_list [] = []

let test2 = sum_list [ 1; 2 ] = [ 1; 3 ]

let test3 = sum_list [ 3; 2; 1; 4 ] = [ 3; 5; 6; 10 ]

(* 16.2 *)
(* 目的: fold_left を作る *)
let rec fold_left f init lst =
  match lst with [] -> init | first :: rest -> fold_left f (f init first) rest

(* テスト コピペ*)
let test1 = fold_left ( - ) 0 [] = 0

let test2 = fold_left ( - ) 10 [ 4; 1; 3 ] = 2

let test3 = fold_left (fun lst a -> a :: lst) [] [ 1; 2; 3; 4 ] = [ 4; 3; 2; 1 ]
