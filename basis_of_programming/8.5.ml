(* 8.5 *)
type ekimei_t = {
  kanji : string;
  kana : string;
  romaji : string;
  shozoku : string;
}

(* 8.6 *)
(* 目的: 駅名を表示する *)
(* hyoji : ekimei_t -> string *)
let hyoji e =
  match e with
  | { kanji; kana; shozoku } -> shozoku ^ ", " ^ kanji ^ "（" ^ kana ^ "）"

let test1 =
  hyoji
    { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" }
  = "丸ノ内線, 茗荷谷（みょうがだに）"

(* 8.7 *)
type ekikan_t = {
  kiten : string;
  shuten : string;
  keiyu : string;
  kyori : float;
  jikan : int;
}
