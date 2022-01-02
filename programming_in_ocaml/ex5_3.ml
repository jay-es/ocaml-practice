(* 5.3 リストを集合とみなして，以下の集合演算をする関数を定義 *)
(* ① mem a s でa がs の要素かどうかを判定する関数mem *)
(* 'a -> 'a list -> bool  *)
let rec mem a = function [] -> false | v :: rest -> v = a || mem a rest

let test1 = mem 1 [ 0; 1; 2 ] = true

let test1 = mem 3 [ 0; 1; 2 ] = false

(* ② intersect s1 s2 でs1 とs2 の共通部分を返す関数intersect *)
(* 'a list -> 'a list -> 'a list *)
let rec intersect lst = function
  | [] -> []
  | v :: rest ->
      let res = intersect lst rest in
      if mem v lst then v :: res else res

let test1 = intersect [ 0; 1; 2 ] [ 2; 3; 4 ] = [ 2 ]

let test2 = intersect [ 0; 1; 2 ] [ 2; 3; 1 ] = [ 2; 1 ]

(* ③ union s1 s2 でs1 とs2 の和を返す関数union *)
(* 'a list -> 'a list -> 'a list *)
let rec union a1 a2 =
  match a1 with
  | [] -> a2
  | v :: rest ->
      let res = union rest a2 in
      if mem v a2 then res else v :: res

let test1 = union [ 0; 1; 2 ] [ 2; 3; 4 ] = [ 0; 1; 2; 3; 4 ]

let test2 = union [ 0; 1; 2 ] [ 2; 3; 1 ] = [ 0; 2; 3; 1 ]

(* ④ diff s1 s2 でs1 とs2 の差を返す関数diff *)
(* 'a list -> 'a list -> 'a list *)
let rec diff lst = function
  | [] -> []
  | v :: rest ->
      let res = diff lst rest in
      if mem v lst then res else v :: res

let test1 = diff [ 0; 1; 2 ] [ 2; 3; 4 ] = [ 3; 4 ]

let test2 = diff [ 0; 1; 2 ] [ 2; 3; 1 ] = [ 3 ]

(* 5.4 *)
(* map f (map g l) をmap を一度だけ使用するように書き換え *)
let f x = x * 2

let g x = x + 1

let l = [ 0; 1; 2; 3; 4 ]

let a1 = List.map f (List.map g l)

let a2 = List.map (fun x -> f (g x)) l

let test1 = a1 = a2

(* 5.5 concat，forall，exists をfold_right を使って定義 *)
let concat lst = List.fold_right (fun v acc -> v @ acc) lst []

let test1 = concat [ [ 0; 3; 4 ]; [ 2 ]; []; [ 5; 0 ] ] = [ 0; 3; 4; 2; 5; 0 ]

let forall f lst = List.fold_right (fun v acc -> acc && f v) lst true

let test1 = forall (fun x -> x >= 5) [ 9; 20; 5 ] = true

let test2 = forall (fun x -> x >= 5) [ 6; 3; 9 ] = false

let exists f lst = List.fold_right (fun v acc -> acc || f v) lst false

let test1 = exists (fun x -> x mod 7 = 0) [ 23; -98; 19; 53 ] = true

let test2 = exists (fun x -> x mod 13 = 0) [ 23; -98; 19; 53 ] = false

(* 5.7 *)
(* 自然数r に対してx2 +y2 = r であるような，(x;y) (ただしx >= y >= 0) の組すべてをリストとして列挙する関数 *)
(* int -> (int*int) list *)
let squares r =
  let rec make_xs n = if n * n > r then [] else n :: make_xs (n + 1) in
  let rec loop = function
    | [] -> []
    | x :: rest ->
        let tmp_y = sqrt (float_of_int (r - (x * x))) in
        let y = int_of_float tmp_y in
        let res = loop rest in
        if Float.is_integer tmp_y && x >= y then (x, y) :: res else res
  in
  loop (make_xs 0)

let squares r =
  let rec loop x =
    if x * x > r then []
    else
      let tmp_y = sqrt (float_of_int (r - (x * x))) in
      let y = int_of_float tmp_y in
      let res = loop (x + 1) in
      if Float.is_integer tmp_y && x >= y then (x, y) :: res else res
  in
  loop 0

let test1 = squares 0 = [ (0, 0) ]

let test2 = List.length (squares 48612265) = 32
