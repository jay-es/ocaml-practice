(* 16.3 *)
(* 問題 14.13 の koushin に引数をひとつ増やす *)

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
(* ;;

   #use "metro.ml" *)

let rec koushin p eki_list ekikan_list =
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
          let kyori = get_ekikan_kyori pn qn ekikan_list in
          if kyori = infinity then q
          else if ps +. kyori < qs then
            { namae = qn; saitan_kyori = ps +. kyori; temae_list = qn :: pt }
          else q)
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

(* 16.4 *)
let saitan_wo_bunri lst =
  let min_eki =
    List.fold_right
      (fun a b -> if a.saitan_kyori <= b.saitan_kyori then a else b)
      lst
      { namae = ""; saitan_kyori = infinity; temae_list = [] }
  in
  (min_eki, List.filter (fun x -> x <> min_eki) lst)

(* 未確定の駅リストと駅間のリストから各駅の最短距離と最短経路が入ったリストを返す *)
(* dijkstra_main: eki_t list -> ekikan_t list -> eki_t list *)
let rec dijkstra_main v ekikan_list =
  if v = [] then []
  else
    let min, others = saitan_wo_bunri v in
    let updated = koushin min others ekikan_list in
    min :: dijkstra_main updated ekikan_list

(* テスト コピペ *)
let test1 = dijkstra_main [] global_ekikan_list = []

let test2 =
  dijkstra_main lst global_ekikan_list
  = [
      { namae = "茗荷谷"; saitan_kyori = 0.; temae_list = [ "茗荷谷" ] };
      { namae = "新大塚"; saitan_kyori = 1.2; temae_list = [ "新大塚"; "茗荷谷" ] };
      { namae = "後楽園"; saitan_kyori = 1.8; temae_list = [ "後楽園"; "茗荷谷" ] };
      { namae = "池袋"; saitan_kyori = 3.; temae_list = [ "池袋"; "新大塚"; "茗荷谷" ] };
    ]

(* 16.5 *)
(* 始点の駅名ローマ字と終点の駅名ローマ字から、最短経路計算済みの終点のレコードを返す *)
(* dijkstra : string -> string -> eki_t *)
let dijkstra kiten_romaji shuten_romaji =
  (* 12.4 *)
  let rec kana_insert lst eki =
    match lst with
    | [] -> [ eki ]
    | ({ kana = k1 } as first) :: rest -> (
        match eki with
        | { kana = k2 } ->
            if k1 = k2 then kana_insert rest eki
            else if k1 < k2 then first :: kana_insert rest eki
            else eki :: lst)
  in
  let rec seiretsu lst =
    match lst with
    | [] -> []
    | first :: rest -> kana_insert (seiretsu rest) first
  in
  (* 10.10 *)
  let rec romaji_to_kanji romaji lst =
    match lst with
    | [] -> ""
    | { kanji = k; romaji = r } :: rest ->
        if r = romaji then k else romaji_to_kanji romaji rest
  in
  (* 14.12 *)
  let make_initial_eki_list lst kiten =
    List.map
      (fun ekimei ->
        match ekimei with
        | { kanji = k } ->
            if k = kiten then
              { namae = k; saitan_kyori = 0.; temae_list = [ k ] }
            else { namae = k; saitan_kyori = infinity; temae_list = [] })
      lst
  in
  let ekimei_list = seiretsu global_ekimei_list in
  let kiten = romaji_to_kanji kiten_romaji ekimei_list in
  let shuten = romaji_to_kanji shuten_romaji ekimei_list in
  let eki_list = make_initial_eki_list ekimei_list kiten in
  let updated_list = dijkstra_main eki_list global_ekikan_list in
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
