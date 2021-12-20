(* 21.3 *)
(* メトロネットワーク最短路問題のプログラムをスタンドアローンにする *)
open Metro

(*
 * 17.14.ml からのコピペ
 * ここから
 *)

type eki_t = { namae : string; saitan_kyori : float; temae_list : string list }

type ekikan_tree_t =
  | Empty
  | Node of ekikan_tree_t * string * (string * float) list * ekikan_tree_t

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

(* from 17.14 *)
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

(*
 * 17.14.ml からのコピペ
 * ここまで
 *)

(* ex21_1 から *)
let print_eki eki =
  match eki with
  | { namae = n; saitan_kyori = s; temae_list = lst } -> (
      match lst with
      | [] -> assert false (* この場合は起こりえない *)
      | [ a ] ->
          print_string (a ^ "（" ^ string_of_float s ^ "km）");
          print_newline ()
      | a :: rest ->
          List.fold_right (fun b () -> print_string (b ^ "、")) rest ();
          print_string (a ^ "（" ^ string_of_float s ^ "km）");
          print_newline ())

let main kiten shuten = print_eki (dijkstra kiten shuten)

let _ = main Sys.argv.(1) Sys.argv.(2)
