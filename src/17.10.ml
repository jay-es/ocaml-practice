type ekikan_t = {
  kiten : string;
  (* 起点 *)
  shuten : string;
  (* 終点 *)
  keiyu : string;
  (* 経由線名 *)
  kyori : float;
  (* 距離 *)
  jikan : int; (* 時間 *)
}

(* 17.10 *)
type ekikan_tree_t =
  | Empty
  | Node of ekikan_tree_t * string * (string * float) list * ekikan_tree_t

(* 17.11 *)
(* 目的: 「駅名」と「駅名と距離の組のリスト」から距離を返す *)
(* assoc : string -> (string * float) list -> float *)
let rec assoc ekimei_0 lst =
  match lst with
  | [] -> infinity
  | (ekimei, kyori) :: rest ->
      if ekimei = ekimei_0 then kyori else assoc ekimei_0 rest

(* テスト *)
let test1 = assoc "後楽園" [ ("新大塚", 1.2); ("後楽園", 1.8) ] = 1.8

let test2 = assoc "池袋" [ ("新大塚", 1.2); ("後楽園", 1.8) ] = infinity

(* 17.12 *)
(* ekikan_t の情報を ekikan_tree_t に追加する *)
let rec insert tree kiten shuten kyori =
  match tree with
  | Empty -> Node (Empty, kiten, [ (shuten, kyori) ], Empty)
  | Node (t1, ekimei, lst, t2) ->
      if ekimei = kiten then Node (t1, ekimei, (shuten, kyori) :: lst, t2)
      else if ekimei < kiten then
        Node (t1, ekimei, lst, insert t2 kiten shuten kyori)
      else Node (insert t1 kiten shuten kyori, ekimei, lst, t2)

(* 目的: ekikan_t の情報を ekikan_tree_t に追加する *)
(* insert_ekikan : ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
let insert_ekikan tree ekikan =
  match ekikan with
  | { kiten; shuten; kyori } ->
      let tree1 = insert tree shuten kiten kyori in
      insert tree1 kiten shuten kyori

(* 駅間の例 コピペ *)
let ekikan1 =
  { kiten = "池袋"; shuten = "新大塚"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 3 }

let ekikan2 =
  { kiten = "新大塚"; shuten = "茗荷谷"; keiyu = "丸ノ内線"; kyori = 1.2; jikan = 2 }

let ekikan3 =
  { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 }

(* テスト コピペ *)
let tree1 = insert_ekikan Empty ekikan1

let test1 =
  tree1
  = Node
      ( Empty,
        "新大塚",
        [ ("池袋", 1.8) ],
        Node (Empty, "池袋", [ ("新大塚", 1.8) ], Empty) )

let tree2 = insert_ekikan tree1 ekikan2

let test2 =
  tree2
  = Node
      ( Empty,
        "新大塚",
        [ ("茗荷谷", 1.2); ("池袋", 1.8) ],
        Node
          ( Empty,
            "池袋",
            [ ("新大塚", 1.8) ],
            Node (Empty, "茗荷谷", [ ("新大塚", 1.2) ], Empty) ) )

let tree3 = insert_ekikan tree2 ekikan3

let test3 =
  tree3
  = Node
      ( Node (Empty, "後楽園", [ ("茗荷谷", 1.8) ], Empty),
        "新大塚",
        [ ("茗荷谷", 1.2); ("池袋", 1.8) ],
        Node
          ( Empty,
            "池袋",
            [ ("新大塚", 1.8) ],
            Node (Empty, "茗荷谷", [ ("後楽園", 1.8); ("新大塚", 1.2) ], Empty) ) )

(* 17.13 *)
(* 目的: ekikan_t list のすべてを ekikan_tree_t に挿入 *)
(* inserts_ekikan : ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)
let inserts_ekikan tree lst = List.fold_left insert_ekikan tree lst

(* テスト コピペ *)
let test1 =
  inserts_ekikan Empty [ ekikan1; ekikan2; ekikan3 ]
  = Node
      ( Node (Empty, "後楽園", [ ("茗荷谷", 1.8) ], Empty),
        "新大塚",
        [ ("茗荷谷", 1.2); ("池袋", 1.8) ],
        Node
          ( Empty,
            "池袋",
            [ ("新大塚", 1.8) ],
            Node (Empty, "茗荷谷", [ ("後楽園", 1.8); ("新大塚", 1.2) ], Empty) ) )
