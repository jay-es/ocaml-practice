type eki_t = { namae : string; saitan_kyori : float; temae_list : string list }

(* 21.1 *)
(* 目的: eki_t の情報をきれいに表示 *)
(* print_eki : eki_t -> unit *)
let print_eki eki =
  match eki with
  | { namae; saitan_kyori } ->
      print_string ("駅名は" ^ namae);
      print_newline ();
      print_string "最短距離は";
      print_float saitan_kyori;
      print_string "です";
      print_newline ()

let test1 =
  print_eki { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] } = ()

(* 21.2 *)
(* 問題15.3の sieve を、呼ばれるたびに引数のリストの長さを表示するように書き換える *)

let rec sieve lst =
  print_int (List.length lst);
  print_newline ();
  match lst with
  | [] -> []
  | first :: rest ->
      first :: sieve (List.filter (fun n -> n mod first <> 0) rest)

(* テスト コピペ *)
let test1 = sieve [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ] = [ 2; 3; 5; 7 ]
