(* append *)
(* リストを連結 *)
(* 'a list -> 'a list -> 'a list *)
let rec append l1 l2 =
  match l1 with [] -> l2 | first :: rest -> first :: append rest l2

let test1 = append [ 1; 2; 3 ] [ 4; 5; 6 ] = [ 1; 2; 3; 4; 5; 6 ]

let test2 = append [ 5; 6 ] [] = [ 5; 6 ]

let test3 = append [] [ true; false ] = [ true; false ]

(* reverse *)
(* リストを反転 *)
(* 'a list -> 'a list -> 'a list *)
let rec reverse = function [] -> [] | v :: rest -> reverse rest @ [ v ]

let rec reverse lst =
  let rec f l1 l2 = match l1 with [] -> l2 | v :: rest -> f rest (v :: l2) in
  f lst []

let test1 = reverse [] = []

let test2 = reverse [ 1; 2; 3 ] = [ 3; 2; 1 ]

let test3 =
  reverse [ [ 1; 2; 3 ]; [ 4; 5 ]; []; [ 6; 7; 8; 9; 10 ] ]
  = [ [ 6; 7; 8; 9; 10 ]; []; [ 4; 5 ]; [ 1; 2; 3 ] ]

(* forall *)
(* リスト内のすべてが条件を満たすか *)
(* ('a -> bool) 'a list -> bool *)
let rec forall f lst =
  match lst with [] -> true | v :: rest -> f v && forall f rest

let forall f lst =
  let rec loop = function [] -> true | v :: rest -> f v && loop rest in
  loop lst

let forall f = function [] -> true | v :: rest -> f v && forall f rest

let test1 = forall (fun x -> x >= 5) [ 9; 20; 5 ] = true

let test2 = forall (fun x -> x >= 5) [ 6; 3; 9 ] = false

(* exists *)
(* リスト内に条件を満たす要素があるか *)
(* ('a -> bool) 'a list -> bool *)
let rec exists f = function [] -> false | v :: rest -> f v || exists f rest

let test1 = exists (fun x -> x mod 7 = 0) [ 23; -98; 19; 53 ] = true

let test2 = exists (fun x -> x mod 13 = 0) [ 23; -98; 19; 53 ] = false

(* fold_right *)
(* 右からの畳込み *)
(* ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let rec fold_right f l e =
  match l with [] -> e | v :: rest -> f v (fold_right f rest e)

let fold_right f l e =
  let rec loop = function [] -> e | v :: rest -> f v (loop rest) in
  loop l

let test1 = fold_right (fun x y -> x + y) [ 3; 5; 7 ] 0 = 15

(* fold_left *)
(* 左からの畳込み *)
(* ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
let fold_left f e l =
  let rec loop tmp = function [] -> tmp | v :: rest -> loop (f tmp v) rest in
  loop e l

(* 模範解答はこれだった *)
let rec fold_left f e l =
  match l with [] -> e | v :: rest -> fold_left f (f e v) rest

let test1 = fold_left (fun x y -> y :: x) [] [ 1; 2; 3 ] = [ 3; 2; 1 ]
