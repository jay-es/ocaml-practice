type ekimei_t = { romaji : string; kanji : string }

type ekikan_t = { kiten : string; shuten : string; kyori : float }

let global_ekimei_list : ekimei_t list = []

let global_ekikan_list : ekikan_t list = []

(* NOTE: 実行する時だけコメントを外す *)
(* ;; #use "metro.ml" *)

(* from 10.11 *)
let rec get_ekikan_kyori eki1 eki2 lst =
  match lst with
  | [] -> infinity
  | { kiten = k; shuten = s; kyori = r } :: rest ->
      if (eki1 = k && eki2 = s) || (eki1 = s && eki2 = k) then r
      else get_ekikan_kyori eki1 eki2 rest

(* from 12.1 *)
type eki_t = { namae : string; saitan_kyori : float; temae_list : string list }

(* 13.6 *)
let koushin1 p q =
  match (p, q) with
  | ( { namae = pn; saitan_kyori = ps; temae_list = pt },
      { namae = qn; saitan_kyori = qs; temae_list = qt } ) ->
      let kyori = get_ekikan_kyori pn qn global_ekikan_list in
      if kyori = infinity then q
      else if ps +. kyori < qs then
        { namae = qn; saitan_kyori = ps +. kyori; temae_list = qn :: pt }
      else q

(* テスト コピペ *)
let eki1 = { namae = "池袋"; saitan_kyori = infinity; temae_list = [] }

let eki2 = { namae = "新大塚"; saitan_kyori = 1.2; temae_list = [ "新大塚"; "茗荷谷" ] }

let eki3 = { namae = "茗荷谷"; saitan_kyori = 0.; temae_list = [ "茗荷谷" ] }

let eki4 = { namae = "後楽園"; saitan_kyori = infinity; temae_list = [] }

let test1 = koushin1 eki3 eki1 = eki1

let test2 = koushin1 eki3 eki2 = eki2

let test3 = koushin1 eki3 eki3 = eki3

let test4 =
  koushin1 eki3 eki4
  = { namae = "後楽園"; saitan_kyori = 1.8; temae_list = [ "後楽園"; "茗荷谷" ] }

let test5 =
  koushin1 eki2 eki1
  = { namae = "池袋"; saitan_kyori = 3.0; temae_list = [ "池袋"; "新大塚"; "茗荷谷" ] }

let test6 = koushin1 eki2 eki2 = eki2

let test7 = koushin1 eki2 eki3 = eki3

let test8 = koushin1 eki2 eki4 = eki4

(* 13.7 *)

(* 目的: 直前に確定した駅 p と、未確定の駅のリストを受け取って更新処理 *)
(* koushin : eki_t -> eki_t list -> eki_t list *)
let rec koushin p lst = List.map (koushin1 p) lst

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
