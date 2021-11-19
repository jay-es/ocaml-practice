type ekimei_t = { romaji : string; kanji : string }

type ekikan_t = { kiten : string; shuten : string; kyori : float }

let global_ekimei_list : ekimei_t list = []

let global_ekikan_list : ekikan_t list = []

(* NOTE: 実行する時だけコメントを外す *)
(* #use "metro.ml" *)

(* 10.10 *)

(* 目的: ローマ字を入力して、駅の漢字表記を返す *)
(* romaji_to_kanji : String ekimei_t list -> String *)
let rec romaji_to_kanji romaji lst =
  match lst with
  | [] -> ""
  | { kanji = k; romaji = r } :: rest ->
      if r = romaji then k else romaji_to_kanji romaji rest

(* テスト *)
let test1 = romaji_to_kanji "not exist" global_ekimei_list = ""

let test2 = romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷"

(* 10.11 *)

(* 目的: 駅名2つを与えたら距離を返す *)
(* get_ekikan_kyori : strnig -> string -> ekikan_t list -> float *)
let rec get_ekikan_kyori eki1 eki2 lst =
  match lst with
  | [] -> infinity
  | { kiten = k; shuten = s; kyori } :: rest ->
      if (k = eki1 && s = eki2) || (k = eki2 && s = eki1) then kyori
      else get_ekikan_kyori eki1 eki2 rest

(* テスト *)
let test1 = get_ekikan_kyori "a" "b" global_ekikan_list = infinity

let test2 = get_ekikan_kyori "茗荷谷" "b" global_ekikan_list = infinity

let test3 = get_ekikan_kyori "a" "茗荷谷" global_ekikan_list = infinity

let test4 = get_ekikan_kyori "新大塚" "後楽園" global_ekikan_list = infinity

let test5 = get_ekikan_kyori "後楽園" "茗荷谷" global_ekikan_list = 1.8

let test6 = get_ekikan_kyori "新大塚" "茗荷谷" global_ekikan_list = 1.2

(* 10.12 *)

(* 目的: ローマ字の駅名2つから、距離をあらわす文字列を返す *)
(* kyori_wo_hyoji : string -> string -> string *)
let kyori_wo_hyoji romaji1 romaji2 =
  let kanji1 = romaji_to_kanji romaji1 global_ekimei_list in
  if kanji1 = "" then romaji1 ^ " という駅は存在しません"
  else
    let kanji2 = romaji_to_kanji romaji2 global_ekimei_list in
    if kanji2 = "" then romaji2 ^ " という駅は存在しません"
    else
      let kyori = get_ekikan_kyori kanji1 kanji2 global_ekikan_list in
      if kyori = infinity then kanji1 ^ "と" ^ kanji2 ^ "はつながっていません"
      else kanji1 ^ "から" ^ kanji2 ^ "までは " ^ string_of_float kyori ^ "km です"

(* テスト *)
let test1 = kyori_wo_hyoji "myogadani" "b" = "b という駅は存在しません"

let test2 = kyori_wo_hyoji "a" "myogadani" = "a という駅は存在しません"

let test3 = kyori_wo_hyoji "shinotsuka" "korakuen" = "新大塚と後楽園はつながっていません"

let test4 = kyori_wo_hyoji "korakuen" "myogadani" = "後楽園から茗荷谷までは 1.8km です"

let test5 = kyori_wo_hyoji "shinotsuka" "myogadani" = "新大塚から茗荷谷までは 1.2km です"
