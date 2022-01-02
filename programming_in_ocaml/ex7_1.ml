(* 7.1 *)
(* 例外機構を使って find を定義 *)
(* 'a -> 'a list -> int option *)
let find x l =
  let rec find' = function
    | [] -> raise Not_found
    | v :: _ when v = x -> 1
    | _ :: rest -> 1 + find' rest
  in
  try Some (find' l) with Not_found -> None

let test1 = find 7 [ 0; 8; 7; 3 ] = Some 3

let test2 = find 9 [ 0; 8; 7; 3 ] = None

(* 7.2 *)
(* 整数リストの要素すべての積を返す関数prod_list を定義 *)
let prod_list lst =
  let rec prod_list' = function
    | [] -> 1
    | v :: rest -> if v = 0 then raise Exit else v * prod_list' rest
  in
  try prod_list' lst with Exit -> 0

let test1 = prod_list [ 2; 3; 4 ] = 24

let test2 = prod_list [ 1; 0; 8; 7; 3 ] = 0

(* 7.3 *)
(* 穴埋め *)
let rec change coins amount =
  match (coins, amount) with
  | _, 0 -> []
  | (c :: rest as coins), total -> (
      if c > total then change rest total
      else
        try c :: change coins (total - c)
        with Failure "change" -> c :: change rest (total - c))
  | _ -> raise (Failure "change")

let test1 = change [ 5; 2 ] 16 = [ 5; 5; 2; 2; 2 ]
