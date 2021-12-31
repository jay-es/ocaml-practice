(* 5.2 *)
(* ① 正の整数n から1 までの整数の降順リストを生成する関数downto1 *)
(* int -> int list *)
let rec downto1 n = if n = 0 then [] else n :: downto1 (n - 1)

let test1 = downto1 6 = [ 6; 5; 4; 3; 2; 1 ]

(* ② 与えられた正の整数のローマ数字表現（文字列）を求める関数roman *)
(* (int*string) list -> int -> string *)
let rec roman pairs num =
  match pairs with
  | [] -> ""
  | (n, chr) :: rest ->
      if num < n then roman rest num else chr ^ roman pairs (num - n)

let test1 =
  roman
    [
      (1000, "M");
      (500, "D");
      (100, "C");
      (50, "L");
      (10, "X");
      (5, "V");
      (1, "I");
    ]
    1984
  = "MDCCCCLXXXIIII"

let test2 =
  roman
    [
      (1000, "M");
      (900, "CM");
      (500, "D");
      (400, "CD");
      (100, "C");
      (90, "XC");
      (50, "L");
      (40, "XL");
      (10, "X");
      (9, "IX");
      (5, "V");
      (4, "IV");
      (1, "I");
    ]
    1984
  = "MCMLXXXIV"

(* ③ 与えられたリストのリストに対し，（内側のリストの）要素の総数を返す関数nested_length *)
(* 'a list list -> int *)
let rec nested_length = function
  | [] -> 0
  | l :: rest -> List.length l + nested_length rest

let test1 = nested_length [ [ 1; 2; 3 ]; [ 4; 5 ]; [ 6 ]; [ 7; 8; 9; 10 ] ] = 10

(* ④ 与えられたリストのリストに対し，内側のリストの要素を並べたリストを返す関数concat *)
(* 'a list list -> 'a list *)
let rec concat = function [] -> [] | l :: rest -> l @ concat rest

let test1 = concat [ [ 0; 3; 4 ]; [ 2 ]; []; [ 5; 0 ] ] = [ 0; 3; 4; 2; 5; 0 ]

(* ⑤ 二つのリスト[a1; ...; an] と[b1; ...; bn] を引数として，[(a1, b1);...; (an, bn)] を返す関数zip *)
(* 'a list -> 'b list -> ('a*'b) list *)
let rec zip a_list b_list =
  match (a_list, b_list) with
  | [], [] | [], _ :: _ | _ :: _, [] -> []
  | a :: a_rest, b :: b_rest -> (a, b) :: zip a_rest b_rest

let test1 =
  zip
    [ 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 ]
    [ true; true; false; true; false; true; false; false; false; true ]
  = [
      (2, true);
      (3, true);
      (4, false);
      (5, true);
      (6, false);
      (7, true);
      (8, false);
      (9, false);
      (10, false);
      (11, true);
    ]

(* ⑥ ペアのリスト[(a1, b1); ...; (an, bn)] を引数として，リストのペア([a1; ...; an], [b1; ...; bn]) を返す関数unzip *)
(* ('a*'b) list -> ('a list * 'b list) *)
let rec unzip = function
  | [] -> ([], [])
  | (a, b) :: rest ->
      let a_rest, b_rest = unzip rest in
      (a :: a_rest, b :: b_rest)

let test1 =
  unzip
    (zip
       [ 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 ]
       [ true; true; false; true; false; true; false; false; false; true ])
  = ( [ 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 ],
      [ true; true; false; true; false; true; false; false; false; true ] )

(* ⑦ リストと，リストの要素上の述語p を満たすすべての要素のリストを返す関数filter *)
(* ('a -> bool) -> 'a list -> 'a list *)
let rec filter f = function
  | [] -> []
  | v :: rest -> if f v then v :: filter f rest else filter f rest

let is_positive x = x > 0

let test1 = filter is_positive [ -9; 0; 2; 5; -3 ] = [ 2; 5 ]

let test2 =
  filter
    (fun l -> List.length l = 3)
    [ [ 1; 2; 3 ]; [ 4; 5 ]; [ 6; 7; 8 ]; [ 9 ] ]
  = [ [ 1; 2; 3 ]; [ 6; 7; 8 ] ]

(* ⑧ 先頭からn 番目までの要素からなる部分リストを取り出す関数take とn 番目までの要素を抜かした部分リストを取り出す関数drop *)
(* int -> 'a list -> 'a list *)
let take n lst =
  let rec loop res = function
    | [] -> res
    | v :: rest -> if List.length res >= n then res else loop (res @ [ v ]) rest
  in
  loop [] lst

let rec take n = function
  | [] -> []
  | v :: rest -> if n = 0 then [] else v :: take (n - 1) rest

let rec drop n = function
  | [] -> []
  | v :: rest ->
      let res = drop (n - 1) rest in
      if n <= 0 then v :: res else res

let ten_to_zero = downto1 10

let test1 = take 8 ten_to_zero = [ 10; 9; 8; 7; 6; 5; 4; 3 ]

let test2 = drop 7 ten_to_zero = [ 3; 2; 1 ]

(* ⑨（空でない）リストのなかから最大値を返す関数max_list *)
(* int list -> int *)
let rec max_list = function [] -> min_int | v :: rest -> max v (max_list rest)

let test1 = max_list [ 7; 9; 0; -5 ] = 9
