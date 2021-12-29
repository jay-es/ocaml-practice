type ekikan_tree_t =
  | Empty
  | Node of ekikan_tree_t * string * (string * float) list * ekikan_tree_t

(* 17.14 *)
(* 目的: 漢字の駅名2つと ekikan_tree_t から2駅間の距離を返す *)
(* get_ekikan_kyori : string -> string -> float *)
let rec get_ekikan_kyori kiten shuten tree =
  match tree with
  | Empty -> infinity
  | Node (t1, ekimei, lst, t2) ->
      if ekimei < kiten then get_ekikan_kyori kiten shuten t2
      else if ekimei > kiten then get_ekikan_kyori kiten shuten t1
      else
        let rec assoc lst =
          match lst with
          | [] -> infinity
          | (ekimei2, kyori) :: rest ->
              if ekimei2 = shuten then kyori else assoc rest
        in
        assoc lst

(* テスト *)
let ekikan_tree =
  Node
    ( Node (Empty, "後楽園", [ ("茗荷谷", 1.8) ], Empty),
      "新大塚",
      [ ("茗荷谷", 1.2); ("池袋", 1.8) ],
      Node
        ( Empty,
          "池袋",
          [ ("新大塚", 1.8) ],
          Node (Empty, "茗荷谷", [ ("後楽園", 1.8); ("新大塚", 1.2) ], Empty) ) )

let test1 = get_ekikan_kyori "茗荷谷" "新大塚" ekikan_tree = 1.2

let test2 = get_ekikan_kyori "茗荷谷" "池袋" ekikan_tree = infinity

let test3 = get_ekikan_kyori "池袋" "新大塚" ekikan_tree = 1.8

(* 17.15 *)
(* 問 16.5 の dijkstra を新しい get_ekikan_kyori で書き換え *)
type ekimei_t = {
  kanji : string;
  kana : string;
  romaji : string;
  shozoku : string;
}

type ekikan_t = {
  kiten : string;
  shuten : string;
  keiyu : string;
  kyori : float;
  jikan : int;
}

type eki_t = { namae : string; saitan_kyori : float; temae_list : string list }

let global_ekimei_list : ekimei_t list = []

let global_ekikan_list : ekikan_t list = []

(* NOTE: 実行する時だけコメントを外す *)
(* ;;#use "metro.ml" *)

(* from 12.4 *)
let rec kana_insert lst eki =
  match lst with
  | [] -> [ eki ]
  | ({ kana = k1 } as first) :: rest -> (
      match eki with
      | { kana = k2 } ->
          if k1 = k2 then kana_insert rest eki
          else if k1 < k2 then first :: kana_insert rest eki
          else eki :: lst)

let rec seiretsu lst =
  match lst with [] -> [] | first :: rest -> kana_insert (seiretsu rest) first

(* from 10.10 *)
let rec romaji_to_kanji romaji lst =
  match lst with
  | [] -> ""
  | { kanji = k; romaji = r } :: rest ->
      if r = romaji then k else romaji_to_kanji romaji rest

(* from 14.12 *)
let make_initial_eki_list lst kiten =
  List.map
    (fun ekimei ->
      match ekimei with
      | { kanji = k } ->
          if k = kiten then { namae = k; saitan_kyori = 0.; temae_list = [ k ] }
          else { namae = k; saitan_kyori = infinity; temae_list = [] })
    lst

(* from 15.4 *)
let saitan_wo_bunri lst =
  let min_eki =
    List.fold_right
      (fun a b -> if a.saitan_kyori <= b.saitan_kyori then a else b)
      lst
      { namae = ""; saitan_kyori = infinity; temae_list = [] }
  in
  (min_eki, List.filter (fun x -> x <> min_eki) lst)

(* from 16.3 *)
let rec koushin p eki_list ekikan_list =
  List.map
    (fun q ->
      match (p, q) with
      | ( { namae = pn; saitan_kyori = ps; temae_list = pt },
          { namae = qn; saitan_kyori = qs; temae_list = qt } ) ->
          let kyori = get_ekikan_kyori pn qn ekikan_list in
          if kyori = infinity then q
          else if ps +. kyori < qs then
            { namae = qn; saitan_kyori = ps +. kyori; temae_list = qn :: pt }
          else q)
    eki_list

(* from 16.4 *)
let rec dijkstra_main v ekikan_list =
  if v = [] then []
  else
    let min, others = saitan_wo_bunri v in
    let updated = koushin min others ekikan_list in
    min :: dijkstra_main updated ekikan_list

(* from 17.12 *)
let rec insert tree kiten shuten kyori =
  match tree with
  | Empty -> Node (Empty, kiten, [ (shuten, kyori) ], Empty)
  | Node (t1, ekimei, lst, t2) ->
      if ekimei = kiten then Node (t1, ekimei, (shuten, kyori) :: lst, t2)
      else if ekimei < kiten then
        Node (t1, ekimei, lst, insert t2 kiten shuten kyori)
      else Node (insert t1 kiten shuten kyori, ekimei, lst, t2)

let insert_ekikan tree ekikan =
  match ekikan with
  | { kiten; shuten; kyori } ->
      let tree1 = insert tree shuten kiten kyori in
      insert tree1 kiten shuten kyori

let inserts_ekikan tree lst = List.fold_left insert_ekikan tree lst

let dijkstra kiten_romaji shuten_romaji =
  let ekimei_list = seiretsu global_ekimei_list in
  let kiten = romaji_to_kanji kiten_romaji ekimei_list in
  let shuten = romaji_to_kanji shuten_romaji ekimei_list in
  let eki_list = make_initial_eki_list ekimei_list kiten in
  let global_ekikan_tree = inserts_ekikan Empty global_ekikan_list in
  let updated_list = dijkstra_main eki_list global_ekikan_tree in
  List.find (fun { namae = n } -> n = shuten) updated_list

(* テスト コピペ *)
let test1 =
  dijkstra "shibuya" "gokokuji"
  = {
      namae = "護国寺";
      saitan_kyori = 9.8;
      temae_list =
        [ "護国寺"; "江戸川橋"; "飯田橋"; "市ヶ谷"; "麹町"; "永田町"; "青山一丁目"; "表参道"; "渋谷" ];
    }

let test2 =
  dijkstra "myogadani" "meguro"
  = {
      namae = "目黒";
      saitan_kyori = 12.7000000000000028;
      temae_list =
        [
          "目黒";
          "白金台";
          "白金高輪";
          "麻布十番";
          "六本木一丁目";
          "溜池山王";
          "永田町";
          "麹町";
          "市ヶ谷";
          "飯田橋";
          "後楽園";
          "茗荷谷";
        ];
    }
