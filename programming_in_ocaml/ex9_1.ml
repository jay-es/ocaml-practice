(* 9.2 *)
(* 以下のシグネチャに合わせて二分探索木を使ったテーブルを作る *)
module type TABLE2 = sig
  type ('a, 'b) t (* = 以下を削除*)

  val empty : ('a, 'b) t

  val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

  val retrieve : 'a -> ('a, 'b) t -> 'b option

  val dump : ('a, 'b) t -> ('a * 'b) list
end

module Table = struct
  type ('a, 'b) t = Lf | Br of ('a * 'b * ('a, 'b) t * ('a, 'b) t)

  let empty = Lf

  let rec add key datum = function
    | Lf -> Br (key, datum, Lf, Lf)
    | Br (key', datum', left, right) ->
        if key > key' then Br (key', datum', left, add key datum right)
        else if key < key' then Br (key', datum', add key datum left, right)
        else Br (key, datum, left, right)

  let rec retrieve key = function
    | Lf -> None
    | Br (key', datum, left, right) ->
        if key > key' then retrieve key right
        else if key < key' then retrieve key left
        else Some datum

  let rec dump = function
    | Lf -> []
    | Br (key, datum, left, right) -> (key, datum) :: (dump left @ dump right)
end

module Table2 : TABLE2 = Table

(* テスト *)
open Table

let d1 = "the first letter of the English alphabet"

let d2 = "the second letter of the English alphabet"

let t1 = add "a" d1 empty

let t2 = add "b" d2 t1

let test1 = t1 = Br ("a", d1, Lf, Lf)

let test2 = t2 = Br ("a", d1, Lf, Br ("b", d2, Lf, Lf))

let test3 = retrieve "XXX" t2 = None

let test4 = retrieve "b" t2 = Some d2

let test5 = dump t2 = [ ("a", d1); ("b", d2) ]
