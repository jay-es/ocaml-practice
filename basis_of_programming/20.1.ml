type color_t = Red | Black

(* 20.1 *)
(* 各節に 'a 型のキーと 'b 型の値、色を示す color_t の値を持つ木 *)
type ('a, 'b) rb_tree_t =
  | Empty
  | Node of (('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a, 'b) rb_tree_t)

(* 20.2 *)
(* 目的: 赤黒木の赤が連続しないように組み換え *)
(* balance : ('a, 'b) rb_tree_t -> ('a, 'b) rb_tree_t *)
let balance tree =
  match tree with
  | Node
      ( Node (Node (tree_a, xk, xv, Red, tree_b), yk, yv, Red, tree_c),
        zk,
        zv,
        Black,
        tree_d )
  | Node
      ( Node (tree_a, xk, xv, Red, Node (tree_b, yk, yv, Red, tree_c)),
        zk,
        zv,
        Black,
        tree_d )
  | Node
      ( tree_a,
        xk,
        xv,
        Black,
        Node (Node (tree_b, yk, yv, Red, tree_c), zk, zv, Red, tree_d) )
  | Node
      ( tree_a,
        xk,
        xv,
        Black,
        Node (tree_b, yk, yv, Red, Node (tree_c, zk, zv, Red, tree_d)) ) ->
      Node
        ( Node (tree_a, xk, xv, Black, tree_b),
          yk,
          yv,
          Red,
          Node (tree_c, zk, zv, Black, tree_d) )
  | _ -> tree

(* テスト *)
let tree_a = Node (Empty, 1, "A", Black, Empty)

let tree_b = Node (Empty, 10, "B", Black, Empty)

let tree_c = Node (Empty, 100, "C", Black, Empty)

let tree_d = Node (Empty, 1000, "D", Black, Empty)

let result =
  Node
    ( Node (tree_a, 5, "X", Black, tree_b),
      50,
      "Y",
      Red,
      Node (tree_c, 500, "Z", Black, tree_d) )

let test1 = balance Empty = Empty

let test2 =
  balance
    (Node
       ( Node (Node (tree_a, 5, "X", Red, tree_b), 50, "Y", Red, tree_c),
         500,
         "Z",
         Black,
         tree_d ))
  = result

let test3 =
  balance
    (Node
       ( Node (tree_a, 5, "X", Red, Node (tree_b, 50, "Y", Red, tree_c)),
         500,
         "Z",
         Black,
         tree_d ))
  = result

let test4 =
  balance
    (Node
       ( tree_a,
         5,
         "X",
         Black,
         Node (Node (tree_b, 50, "Y", Red, tree_c), 500, "Z", Red, tree_d) ))
  = result

let test5 =
  balance
    (Node
       ( tree_a,
         5,
         "X",
         Black,
         Node (tree_b, 50, "Y", Red, Node (tree_c, 500, "Z", Red, tree_d)) ))
  = result

(* 20.3 *)
(* 目的: 赤黒木にキーと値を挿入 *)
(* insert : ('a, 'b) rb_tree_t -> 'a -> 'b -> ('a, 'b) rb_tree_t *)
let insert tree key value =
  let rec hojo tree2 =
    match tree2 with
    | Empty -> Node (Empty, key, value, Red, Empty)
    | Node (left, k, v, c, right) ->
        if k > key then balance (Node (hojo left, k, v, c, right))
        else if k < key then balance (Node (left, k, v, c, hojo right))
        else Node (left, key, value, c, right)
  in
  match hojo tree with
  | Empty -> assert false (* 絶対に空ではない *)
  | Node (left, k, v, c, right) -> Node (left, k, v, Black, right)

(* テスト コピペ*)
let rb_tree0 = Empty

let rb_tree1 = insert rb_tree0 10 "x"

let rb_tree2 = insert rb_tree1 13 "y"

let rb_tree3 = insert rb_tree2 15 "z"

let test1 = rb_tree1 = Node (Empty, 10, "x", Black, Empty)

let test2 =
  rb_tree2 = Node (Empty, 10, "x", Black, Node (Empty, 13, "y", Red, Empty))

let test3 =
  rb_tree3
  = Node
      ( Node (Empty, 10, "x", Black, Empty),
        13,
        "y",
        Black,
        Node (Empty, 15, "z", Black, Empty) )

(* 20.4 *)
(* 目的: 赤黒木の中から、指定したキーの値を探す *)
(* search : ('a, 'b) rb_tree_t -> 'a -> 'b *)
let rec search tree key =
  match tree with
  | Empty -> raise Not_found
  | Node (left, k, v, c, right) ->
      if k = key then v
      else if k > key then search left key
      else search right key

(* テスト *)
let test1 = try search Empty 1 with Not_found -> 0 = 0

let test2 = search tree_a 1 = "A"

let test3 = search result 1 = "A"

let test4 = search result 5 = "X"
