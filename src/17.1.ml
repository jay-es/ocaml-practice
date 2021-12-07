(* 17.1 *)

type nengou_t = Meiji of int | Taisho of int | Showa of int | Heisei of int

let to_seireki nengou =
  match nengou with
  | Meiji n -> n + 1867
  | Taisho n -> n + 1911
  | Showa n -> n + 1925
  | Heisei n -> n + 1988

(* 目的: 誕生年と現在の年を nentou_t で受け取り、年齢を返す *)
(* nenrei : nengou_t -> nengou_t -> int *)
let nenrei tanjou genzai = to_seireki genzai - to_seireki tanjou

(* テスト コピペ *)
let test1 = nenrei (Showa 42) (Heisei 18) = 39

let test2 = nenrei (Heisei 11) (Heisei 18) = 7

let test3 = nenrei (Meiji 41) (Heisei 17) = 97

(* 17.2 *)
(* 1-12月の型 *)
type year_t =
  | January of int
  | February of int
  | March of int
  | April of int
  | May of int
  | June of int
  | July of int
  | August of int
  | September of int
  | October of int
  | November of int
  | December of int

(* 17.3 *)
(* 12星座の型 *)
type seiza_t =
  | Aries (* 牡羊座	3月21日～4月19日 *)
  | Taurus (* 牡牛座	4月20日～5月20日 *)
  | Gemini (* 双子座	5月21日～6月21日 *)
  | Cancer (* 蟹座	6月22日～7月22日 *)
  | Leo (* 獅子座	7月23日～8月22日 *)
  | Virgo (* 乙女座	8月23日～9月22日 *)
  | Libra (* 天秤座	9月23日～10月23日 *)
  | Scorpio (* 蠍座	10月24日～11月22日 *)
  | Sagittarius (* 射手座	11月23日～12月21日 *)
  | Capricorn (* 山羊座	12月22日～1月19日 *)
  | Aquarius (* 水瓶座	1月20日～2月18日 *)
  | Pisces (* 魚座	2月19日～3月20日 *)
  | None

(* 17.4 *)
(* 目的: year_t から seiza_t を返す *)
(* seiza : year_t -> seiza_t *)
let seiza year =
  match year with
  | January n -> if n <= 19 then Capricorn else Aquarius
  | February n -> if n <= 18 then Aquarius else Pisces
  | March n -> if n <= 20 then Pisces else Aries
  | April n -> if n <= 19 then Aries else Taurus
  | May n -> if n <= 20 then Taurus else Gemini
  | June n -> if n <= 21 then Gemini else Cancer
  | July n -> if n <= 22 then Cancer else Leo
  | August n -> if n <= 22 then Leo else Virgo
  | September n -> if n <= 22 then Virgo else Libra
  | October n -> if n <= 23 then Libra else Scorpio
  | November n -> if n <= 22 then Scorpio else Sagittarius
  | December n -> if n <= 21 then Sagittarius else Capricorn

(* テスト コピペ *)
let test1 = seiza (June 11) = Gemini

let test2 = seiza (June 30) = Cancer

let test3 = seiza (September 17) = Virgo

let test4 = seiza (October 7) = Libra
