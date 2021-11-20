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

(* 12.1 *)
type eki_t = { namae : string; saitan_kyori : float; temae_list : string list }

(* 12.2 *)

(* 目的: ekimei_t のリストから eki_t のリストを作る *)
(* make_eki_list : ekimei_t list -> eki_t -> list *)
let rec make_eki_list lst =
  match lst with
  | [] -> []
  | { kanji = k } :: rest ->
      { namae = k; saitan_kyori = infinity; temae_list = [] }
      :: make_eki_list rest

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

(* 12.3 *)

(* 目的: eki_t リストの初期化 *)
(* shokika : eki_t list -> eki_t list *)
let rec shokika lst kiten =
  match lst with
  | [] -> []
  | ({ namae = n } as first : eki_t) :: rest ->
      (if n = kiten then { namae = n; saitan_kyori = 0.; temae_list = [ n ] }
      else first)
      :: shokika rest kiten

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

(* 12.4 *)

(* 目的: 駅名のリストをひらがな順にソート + 重複削除 *)

(* kana_insert : ekimei_t list -> ekimei_t -> ekimei_t list *)
let rec kana_insert lst eki =
  match lst with
  | [] -> [ eki ]
  | ({ kana = k1 } as first) :: rest -> (
      match eki with
      | { kana = k2 } ->
          if k1 = k2 then kana_insert rest eki
          else if k1 < k2 then first :: kana_insert rest eki
          else eki :: lst)

(* seiretsu : ekimei_t list -> ekimei_t list *)
let rec seiretsu lst =
  match lst with [] -> [] | first :: rest -> kana_insert (seiretsu rest) first

(* テスト *)
let test1 = seiretsu [ eki2; eki1 ] = [ eki1; eki2 ]

let test2 = seiretsu [ eki1; eki2; eki3 ] = [ eki3; eki1; eki2 ]

let test3 = seiretsu [ eki1; eki2; eki3; eki22 ] = [ eki3; eki1; eki2 ]
