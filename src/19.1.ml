(* ２分木を表すモジュール *)
module Tree = struct
  (* ２分木を表す型 *)
  type ('a, 'b) t = Empty | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t

  (* 空の木 *)
  let empty = Empty

  (* 目的：tree にキーが k で値が v を挿入した木を返す *)
  (* insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t *)
  let rec insert tree k v =
    match tree with
    | Empty -> Node (Empty, k, v, Empty)
    | Node (left, key, value, right) ->
        if k = key then Node (left, key, v, right)
        else if k < key then Node (insert left k v, key, value, right)
        else Node (left, key, value, insert right k v)

  (* 目的：tree の中のキー k に対応する値を探して返す *)
  (* みつからなければ例外 Not_found を起こす *)
  (* search : ('a, 'b) t -> 'a -> 'b *)
  let rec search tree k =
    match tree with
    | Empty -> raise Not_found
    | Node (left, key, value, right) ->
        if k = key then value
        else if k < key then search left k
        else search right k
end

(* 19.1 *)
(* get_ekikan_kyori を Tree を使うように書き換え *)
(* 目的: 漢字の駅名2つと ekikan_tree_t から2駅間の距離を返す *)
(* get_ekikan_kyori : string -> string -> float *)
let rec get_ekikan_kyori kiten shuten tree =
  match tree with
  | Tree.Empty -> raise Not_found
  | Node (t1, ekimei, lst, t2) ->
      if ekimei < kiten then get_ekikan_kyori kiten shuten t2
      else if ekimei > kiten then get_ekikan_kyori kiten shuten t1
      else
        let rec assoc lst =
          match lst with
          | [] -> raise Not_found
          | (ekimei2, kyori) :: rest ->
              if ekimei2 = shuten then kyori else assoc rest
        in
        assoc lst

(* テスト *)
let ekikan_tree =
  Tree.Node
    ( Node (Empty, "後楽園", [ ("茗荷谷", 1.8) ], Empty),
      "新大塚",
      [ ("茗荷谷", 1.2); ("池袋", 1.8) ],
      Node
        ( Empty,
          "池袋",
          [ ("新大塚", 1.8) ],
          Node (Empty, "茗荷谷", [ ("後楽園", 1.8); ("新大塚", 1.2) ], Empty) ) )

let test1 = get_ekikan_kyori "茗荷谷" "新大塚" ekikan_tree = 1.2

let test2 =
  try get_ekikan_kyori "茗荷谷" "池袋" ekikan_tree with Not_found -> infinity

let test3 = get_ekikan_kyori "池袋" "新大塚" ekikan_tree = 1.8
