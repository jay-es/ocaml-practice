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
(* ;; #use "metro.ml" *)

(* 18.4 *)
(* 問題 10.11 の get_ekikan_kyori で、つながっていなかったら Not_found を返すように*)
(* 目的: 駅名2つを与えたら距離を返す *)
(* get_ekikan_kyori : strnig -> string -> ekikan_t list -> float *)
let rec get_ekikan_kyori eki1 eki2 lst =
  match lst with
  | [] -> raise Not_found
  | { kiten = k; shuten = s; kyori } :: rest ->
      if (k = eki1 && s = eki2) || (k = eki2 && s = eki1) then kyori
      else get_ekikan_kyori eki1 eki2 rest

(* テスト *)
let test1 =
  try get_ekikan_kyori "a" "b" global_ekikan_list with Not_found -> infinity

let test2 =
  try get_ekikan_kyori "茗荷谷" "b" global_ekikan_list with Not_found -> infinity

let test3 =
  try get_ekikan_kyori "a" "茗荷谷" global_ekikan_list with Not_found -> infinity

let test4 =
  try get_ekikan_kyori "新大塚" "後楽園" global_ekikan_list
  with Not_found -> infinity

let test5 = get_ekikan_kyori "後楽園" "茗荷谷" global_ekikan_list = 1.8

let test6 = get_ekikan_kyori "新大塚" "茗荷谷" global_ekikan_list = 1.2

(* 18.5 *)
(* 問題 16.3 の koushin を ↑ の get_ekikan_kyori を使うように変更 *)

let rec koushin p eki_list ekikan_list =
  List.map
    (fun q ->
      match (p, q) with
      | ( { namae = pn; saitan_kyori = ps; temae_list = pt },
          { namae = qn; saitan_kyori = qs; temae_list = qt } ) -> (
          try
            let kyori = get_ekikan_kyori pn qn ekikan_list in
            if ps +. kyori < qs then
              { namae = qn; saitan_kyori = ps +. kyori; temae_list = qn :: pt }
            else q
          with Not_found -> q))
    eki_list

(* テスト コピペ *)
let eki1 = { namae = "池袋"; saitan_kyori = infinity; temae_list = [] }

let eki2 = { namae = "新大塚"; saitan_kyori = 1.2; temae_list = [ "新大塚"; "茗荷谷" ] }

let eki3 = { namae = "茗荷谷"; saitan_kyori = 0.; temae_list = [ "茗荷谷" ] }

let eki4 = { namae = "後楽園"; saitan_kyori = infinity; temae_list = [] }

(* 駅リストの例 *)
let lst = [ eki1; eki2; eki3; eki4 ]

(* テスト *)
let test1 = koushin eki2 [] global_ekikan_list = []

let test2 =
  koushin eki2 lst global_ekikan_list
  = [
      { namae = "池袋"; saitan_kyori = 3.0; temae_list = [ "池袋"; "新大塚"; "茗荷谷" ] };
      eki2;
      eki3;
      eki4;
    ]

(* 18.6 *)
(* 駅が存在しないことを示す例外 *)
exception No_such_station of string

(* 18.7 *)
(* romaji_to_kanji で ↑ を使うように書き換え *)

(* romaji_to_kanji : string ekimei_t list -> string *)
let rec romaji_to_kanji romaji lst =
  match lst with
  | [] -> raise (No_such_station romaji)
  | { kanji = k; romaji = r } :: rest ->
      if r = romaji then k else romaji_to_kanji romaji rest

(* テスト *)
let test1 =
  try romaji_to_kanji "not exist" global_ekimei_list
  with No_such_station romaji -> romaji

let test2 = romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷"
