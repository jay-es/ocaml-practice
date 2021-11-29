(* 14.11-1 *)
(* 問題 12.2 を map と名前のない関数で書き直す *)
type ekimei_t = {
  kanji : string;
  kana : string;
  romaji : string;
  shozoku : string;
}

let eki1 =
  { kanji = "新大塚"; kana = "しんおおつか"; romaji = "shinotsuka"; shozoku = "丸ノ内線" }

let eki2 =
  { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" }

let eki22 =
  { kanji = "茗荷谷2"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" }

let eki3 =
  { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線" }

type eki_t = { namae : string; saitan_kyori : float; temae_list : string list }

(* 目的: ekimei_t のリストから eki_t のリストを作る *)
(* make_eki_list : ekimei_t list -> eki_t -> list *)
let rec make_eki_list lst =
  List.map
    (fun ekimei ->
      match ekimei with
      | { kanji = k } -> { namae = k; saitan_kyori = infinity; temae_list = [] })
    lst

(* テスト *)
let test1 =
  make_eki_list [ eki2 ]
  = [ { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] } ]

let test2 =
  make_eki_list [ eki1; eki2 ]
  = [
      { namae = "新大塚"; saitan_kyori = infinity; temae_list = [] };
      { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] };
    ]

(* 14.11-2 *)
(* 問題 12.3 を map と名前のない関数で書き直す *)

(* 目的: eki_t リストの初期化 *)
(* shokika : eki_t list -> string -> eki_t list *)
let rec shokika lst kiten =
  List.map
    (fun eki ->
      match eki with
      | { namae = n } ->
          if n = kiten then { namae = n; saitan_kyori = 0.; temae_list = [ n ] }
          else eki)
    lst

(* テスト *)
let test1 =
  shokika (make_eki_list [ eki2 ]) "茗荷谷"
  = [ { namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = [ "茗荷谷" ] } ]

let test2 =
  shokika (make_eki_list [ eki1; eki2; eki3 ]) "後楽園"
  = [
      { namae = "新大塚"; saitan_kyori = infinity; temae_list = [] };
      { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] };
      { namae = "後楽園"; saitan_kyori = 0.0; temae_list = [ "後楽園" ] };
    ]

(* 14.12 *)
(* 目的: make_eki_list と shokika を一つの関数に *)
(* make_initial_eki_list : ekimei_t list -> string -> eki_t list *)
let make_initial_eki_list lst kiten =
  List.map
    (fun ekimei ->
      match ekimei with
      | { kanji = k } ->
          if k = kiten then { namae = k; saitan_kyori = 0.; temae_list = [ k ] }
          else { namae = k; saitan_kyori = infinity; temae_list = [] })
    lst

(* テスト *)
let test1 =
  make_initial_eki_list [ eki2 ] "茗荷谷"
  = [ { namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = [ "茗荷谷" ] } ]

let test2 =
  make_initial_eki_list [ eki1; eki2; eki3 ] "後楽園"
  = [
      { namae = "新大塚"; saitan_kyori = infinity; temae_list = [] };
      { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] };
      { namae = "後楽園"; saitan_kyori = 0.0; temae_list = [ "後楽園" ] };
    ]

(* 14.13 *)
(* 問題 14.7 を名前のない関数で書き直す *)

type ekikan_t = { kiten : string; shuten : string; kyori : float }

let global_ekikan_list : ekikan_t list = []

(* NOTE: 実行する時だけコメントを外す *)
(* ;; #use "metro.ml" *)

(* koushin : eki_t -> eki_t list -> eki_t list *)
let rec koushin p lst =
  let rec get_ekikan_kyori eki1 eki2 lst =
    match lst with
    | [] -> infinity
    | { kiten = k; shuten = s; kyori = r } :: rest ->
        if (eki1 = k && eki2 = s) || (eki1 = s && eki2 = k) then r
        else get_ekikan_kyori eki1 eki2 rest
  in
  List.map
    (fun q ->
      match (p, q) with
      | ( { namae = pn; saitan_kyori = ps; temae_list = pt },
          { namae = qn; saitan_kyori = qs; temae_list = qt } ) ->
          let kyori = get_ekikan_kyori pn qn global_ekikan_list in
          if kyori = infinity then q
          else if ps +. kyori < qs then
            { namae = qn; saitan_kyori = ps +. kyori; temae_list = qn :: pt }
          else q)
    lst

(* テスト コピペ *)
let eki1 = { namae = "池袋"; saitan_kyori = infinity; temae_list = [] }

let eki2 = { namae = "新大塚"; saitan_kyori = 1.2; temae_list = [ "新大塚"; "茗荷谷" ] }

let eki3 = { namae = "茗荷谷"; saitan_kyori = 0.; temae_list = [ "茗荷谷" ] }

let eki4 = { namae = "後楽園"; saitan_kyori = infinity; temae_list = [] }

(* 駅リストの例 *)
let lst = [ eki1; eki2; eki3; eki4 ]

(* テスト *)
let test1 = koushin eki2 [] = []

let test2 =
  koushin eki2 lst
  = [
      { namae = "池袋"; saitan_kyori = 3.0; temae_list = [ "池袋"; "新大塚"; "茗荷谷" ] };
      eki2;
      eki3;
      eki4;
    ]
