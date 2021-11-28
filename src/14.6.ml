(* 14.6 *)
(* 目的: 13.1 節の count を filter と length で 定義 *)
type gakusei_t = { namae : string; seiseki : string }

let gakusei_list =
  [
    { namae = "Alice"; seiseki = "A" };
    { namae = "Bob"; seiseki = "B" };
    { namae = "Charlie"; seiseki = "A" };
  ]

(* count : gakusei_t -> string -> int *)
let count lst seiseki0 =
  let is_seiseki0 gakusei =
    match gakusei with { namae = n; seiseki = s } -> s = seiseki0
  in
  List.length (List.filter is_seiseki0 lst)

(* テスト *)
let test1 = count gakusei_list "C" = 0

let test2 = count gakusei_list "B" = 1

let test2 = count gakusei_list "A" = 2

(* 14.7 *)
(* 目的: 問題 13.7 を局所定義で書き直す *)
type ekikan_t = { kiten : string; shuten : string; kyori : float }

type eki_t = { namae : string; saitan_kyori : float; temae_list : string list }

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
  let koushin1 p q =
    match (p, q) with
    | ( { namae = pn; saitan_kyori = ps; temae_list = pt },
        { namae = qn; saitan_kyori = qs; temae_list = qt } ) ->
        let kyori = get_ekikan_kyori pn qn global_ekikan_list in
        if kyori = infinity then q
        else if ps +. kyori < qs then
          { namae = qn; saitan_kyori = ps +. kyori; temae_list = qn :: pt }
        else q
  in
  List.map (koushin1 p) lst

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
