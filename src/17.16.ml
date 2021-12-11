type eki_t = { namae : string; saitan_kyori : float; temae_list : string list }

(* 17.16 *)
(* 問15.5 の saitan_wo_bunri を書き換え *)
(* saitan_wo_bunri : eki_t -> eki_t list -> eki_t * eki_t list *)
let saitan_wo_bunri eki lst =
  List.fold_right
    (fun cur (p, v) ->
      match (cur, p) with
      | { saitan_kyori = cs }, { saitan_kyori = ps } ->
          if cs < ps then (cur, p :: v) else (p, cur :: v))
    lst (eki, [])

(* 17.17 *)
(* minimum も書き換え *)
(* minimum : int -> int list -> int *)
let rec minimum i lst =
  match lst with [] -> i | first :: rest -> minimum (min first i) rest

(* テスト *)
let test1 = minimum 1 [ 2; 3 ] = 1

let test1 = minimum 3 [ 2; 1 ] = 1
