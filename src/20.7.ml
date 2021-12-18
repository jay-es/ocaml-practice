(* 20.7 *)
(* 集合のモジュールを実装 *)
module type Set_t = sig
  type 'a t

  val empty : 'a t

  val singleton : 'a -> 'a t

  val union : 'a t -> 'a t -> 'a t

  val inter : 'a t -> 'a t -> 'a t

  val diff : 'a t -> 'a t -> 'a t

  val mem : 'a -> 'a t -> bool
end

module Set (* : Set_t *) = struct
  type 'a t = 'a list

  let empty = []

  let singleton v = [ v ]

  let union a b = a @ b

  let inter a b = List.filter (fun x -> List.mem x b) a

  let diff a b =
    let a2 = List.filter (fun x -> not (List.mem x b)) a in
    let b2 = List.filter (fun x -> not (List.mem x a)) b in
    a2 @ b2

  let mem = List.mem
end

(* テスト *)
let set1 = [ 2; 4; 6 ]

let set2 = [ 3; 6; 9 ]

let test1 = Set.union set1 set2 = [ 2; 4; 6; 3; 6; 9 ]

let test2 = Set.inter set1 set2 = [ 6 ]

let test3 = Set.diff set1 set2 = [ 2; 4; 3; 9 ]

let test4 = Set.mem 2 set1 = true

let test5 = Set.mem 3 set1 = false
