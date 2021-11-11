(* 8.1 *)
type book_t = {
  title : string;
  author : string;
  publisher : string;
  price : int;
  isbn : string;
}
(* データは割愛 *)

(* 8.2 *)
type okozukai_t = {
  item : string;
  price : int;
  shop : string;
  date : int * int * int;
}

(* 8.3 *)
type person_t = {
  name : string;
  height : float;
  weight : float;
  birthday : int * int;
  blood_type : string;
}

(* 8.4 *)

(* 目的: 人のデータを受け取って、名前と血液型を返す *)
(* ketsueki_hyoji : person_t -> string *)
let ketsueki_hyoji p =
  match p with
  | { name = n; height = h; weight = w; birthday = bd; blood_type = bt } ->
      n ^ "さんの血液型は" ^ bt ^ "です"

let test1 =
  ketsueki_hyoji
    {
      name = "Alice";
      height = 0.;
      weight = 0.;
      birthday = (1, 1);
      blood_type = "A";
    }
  = "Aliceさんの血液型はAです"

let test2 =
  ketsueki_hyoji
    {
      name = "Bob";
      height = 0.;
      weight = 0.;
      birthday = (1, 1);
      blood_type = "B";
    }
  = "Bobさんの血液型はBです"
