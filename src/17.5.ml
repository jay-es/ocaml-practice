(* 17.5 *)
type tree_t = Empty | Leaf of int | Node of tree_t * int * tree_t

(* 目的: tree_t の木を受け取って、節や葉に入っている値を2倍にする *)
(* tree_double : tree_t -> tree_t *)
let rec tree_double tree =
  match tree with
  | Empty -> Empty
  | Leaf n -> Leaf (n * 2)
  | Node (t1, n, t2) -> Node (tree_double t1, n * 2, tree_double t2)

(* テスト *)
let tree1 = Empty

let tree2 = Leaf 2

let tree3 = Node (tree1, 5, tree2)

let test1 = tree_double tree1 = Empty

let test2 = tree_double tree2 = Leaf 4

let test3 = tree_double tree3 = Node (Empty, 10, Leaf 4)

(* 17.6 *)
(* 目的: 木のすべてに関数を適用する *)
(* tree_map : (int -> int) -> tree_t -> tree_t *)
let rec tree_map f tree =
  match tree with
  | Empty -> Empty
  | Leaf n -> Leaf (f n)
  | Node (t1, n, t2) -> Node (tree_map f t1, f n, tree_map f t2)

(* テスト *)
let add3 n = n + 3

let test1 = tree_map add3 tree1 = Empty

let test2 = tree_map add3 tree2 = Leaf 5

let test3 = tree_map add3 tree3 = Node (Empty, 8, Leaf 5)

(* 17.7 *)
(* 目的: 木に節と葉がいくつあるか返す *)
(* tree_length : tree_t -> int *)
let rec tree_length tree =
  match tree with
  | Empty -> 0
  | Leaf n -> 1
  | Node (t1, n, t2) -> tree_length t1 + 1 + tree_length t2

(* テスト *)
let test1 = tree_length tree1 = 0

let test2 = tree_length tree2 = 1

let test3 = tree_length tree3 = 2

(* 17.8 *)
(* 目的: 木の深さを返す *)
(* tree_depth : tree_t -> int *)
let rec tree_depth tree =
  match tree with
  | Empty -> 0
  | Leaf n -> 0
  | Node (t1, n, t2) -> 1 + max (tree_depth t1) (tree_depth t2)

(* テスト *)
let test1 = tree_depth tree1 = 0

let test2 = tree_depth tree2 = 1

let test3 = tree_depth tree3 = 2
