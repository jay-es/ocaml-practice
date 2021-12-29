(* 18.1 *)
type person_t = {
  name : string;
  (* height : float; *)
  (* weight : float; *)
  (* birthday : int * int; *)
  blood_type : string;
}

(* 目的: 最初のA型の人を返す *)
(* first_A : person_t list -> person_t option *)
let rec first_A lst =
  match lst with
  | [] -> None
  | ({ blood_type } as first) :: rest ->
      if blood_type = "A" then Some first else first_A rest

(* テスト *)
let alice = { name = "Alice"; blood_type = "A" }

let bob = { name = "Bob"; blood_type = "B" }

let charlie = { name = "Charlie"; blood_type = "A" }

let test1 = first_A [] = None

let test2 = first_A [ alice ] = Some alice

let test3 = first_A [ bob ] = None

let test4 = first_A [ bob; alice; charlie ] = Some alice

(* 18.2 *)
let rec price item yaoya_list =
  match yaoya_list with
  | [] -> None
  | (yasai, nedan) :: rest ->
      if item = yasai then Some nedan else price item rest

(* 目的: 野菜のリストと八百屋のリストから、八百屋においていない野菜の数を返す *)
(* count_urikire_yasai : string list -> (string * int) list -> int *)
let rec count_urikire_yasai yasai_list yaoya_list =
  match yasai_list with
  | [] -> 0
  | first :: rest -> (
      match price first yaoya_list with
      | None -> 1 + count_urikire_yasai rest yaoya_list
      | Some n -> count_urikire_yasai rest yaoya_list)

(* テスト コピペ *)
let yaoya_list = [ ("トマト", 300); ("たまねぎ", 200); ("にんじん", 150); ("ほうれん草", 200) ]

let test1 = count_urikire_yasai [ "たまねぎ"; "にんじん" ] yaoya_list = 0

let test2 = count_urikire_yasai [ "たまねぎ"; "じゃがいも"; "にんじん" ] yaoya_list = 1

let test3 = count_urikire_yasai [ "しいたけ"; "なす"; "にんじん" ] yaoya_list = 2

(* 18.3 *)
(* 問題17.11を見つからなかった場合は例外が起こるように *)
let rec assoc ekimei_0 lst =
  match lst with
  | [] -> raise Not_found
  | (ekimei, kyori) :: rest ->
      if ekimei = ekimei_0 then kyori else assoc ekimei_0 rest
