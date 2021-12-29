(* 15.4 *)
type eki_t = { namae : string; saitan_kyori : float; temae_list : string list }

let eki1 = { namae = "新大塚"; saitan_kyori = 1.0; temae_list = [] }

let eki2 = { namae = "茗荷谷"; saitan_kyori = 2.0; temae_list = [] }

let eki3 = { namae = "後楽園"; saitan_kyori = 3.0; temae_list = [] }

(* 目的: リストを最短距離最小とそれ以外に分ける *)
(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)
let saitan_wo_bunri lst =
  let min_eki =
    List.fold_right
      (fun a b -> if a.saitan_kyori <= b.saitan_kyori then a else b)
      lst
      { namae = ""; saitan_kyori = infinity; temae_list = [] }
  in
  (min_eki, List.filter (fun x -> x <> min_eki) lst)

(* パターンマッチで書き直したバージョン *)
let saitan_wo_bunri lst =
  let min_eki =
    List.fold_right
      (fun a b ->
        match (a, b) with
        | { saitan_kyori = a_s }, { saitan_kyori = b_s } ->
            if a_s <= b_s then a else b)
      lst
      { namae = ""; saitan_kyori = infinity; temae_list = [] }
  in
  (min_eki, List.filter (fun x -> x <> min_eki) lst)

(* テスト *)
let test1 =
  saitan_wo_bunri []
  = ({ namae = ""; saitan_kyori = infinity; temae_list = [] }, [])

let test2 = saitan_wo_bunri [ eki1; eki2 ] = (eki1, [ eki2 ])

let test3 = saitan_wo_bunri [ eki1; eki2; eki3 ] = (eki1, [ eki2; eki3 ])

(* 15.5 *)
(* 問題 15.4 を fold_right で使って書き直す*)
let saitan_wo_bunri lst =
  match lst with
  | [] -> ({ namae = ""; saitan_kyori = infinity; temae_list = [] }, [])
  | first :: rest ->
      List.fold_right
        (fun cur (p, v) ->
          match (cur, p) with
          | { saitan_kyori = cs }, { saitan_kyori = ps } ->
              if cs < ps then (cur, p :: v) else (p, cur :: v))
        rest (first, [])

(* テスト *)
let test1 =
  saitan_wo_bunri []
  = ({ namae = ""; saitan_kyori = infinity; temae_list = [] }, [])

let test2 = saitan_wo_bunri [ eki1; eki2 ] = (eki1, [ eki2 ])

let test3 = saitan_wo_bunri [ eki1; eki2; eki3 ] = (eki1, [ eki2; eki3 ])
