module Color = struct
  type t = Spade | Heart | Diamond | Club

  let all = [Spade; Heart; Diamond; Club]

  let toString = function
    | Spade -> "S"
    | Heart -> "H"
    | Diamond -> "D"
    | Club -> "C"

  let toStringVerbose = function
    | Spade -> "Spade"
    | Heart -> "Heart"
    | Diamond -> "Diamond"
    | Club -> "Club"

end

module Value = struct
  type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | Ace

  let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; Ace]

  let toInt = function
    | T2 -> 1
    | T3 -> 2
    | T4 -> 3
    | T5 -> 4
    | T6 -> 5
    | T7 -> 6
    | T8 -> 7
    | T9 -> 8
    | T10 -> 9
    | Jack -> 10
    | Queen -> 11
    | King -> 12
    | Ace -> 13

  let toString = function
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | Ace -> "A"

  let toStringVerbose = function
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King"
    | Ace -> "Ace"

  let next = function
    | T2 -> T3
    | T3 -> T4
    | T4 -> T5
    | T5 -> T6
    | T6 -> T7
    | T7 -> T8
    | T8 -> T9
    | T9 -> T10
    | T10 -> Jack
    | Jack -> Queen
    | Queen -> King
    | King -> Ace
    | Ace -> invalid_arg "Value.next: Ace has no next value"

  let previous = function
    | T2 -> invalid_arg "Value.previous: T2 has no previous value"
    | T3 -> T2
    | T4 -> T3
    | T5 -> T4
    | T6 -> T5
    | T7 -> T6
    | T8 -> T7
    | T9 -> T8
    | T10 -> T9
    | Jack -> T10
    | Queen -> Jack
    | King -> Queen
    | Ace -> King

end

type card = (Value.t * Color.t)

(* val newCard : Value.t -> Color.t -> t *)
let newCard v c =
  (v, c)

(* val allSpades : t list *)
let allSpades = List.map (fun x -> newCard x Color.Spade) Value.all

(* val allHearts : t list *)
let allHearts = List.map (fun x -> newCard x Color.Heart) Value.all

(* val allDiamonds : t list *)
let allDiamonds = List.map (fun x -> newCard x Color.Diamond) Value.all

(* val allClubs : t list *)
let allClubs = List.map (fun x -> newCard x Color.Club) Value.all

(* val all : t list *)
let all = allSpades @ allHearts @ allDiamonds @ allClubs

(* val getValue : t -> Value.t *)
let getValue (v, c) = v

(* val getColor : t -> Color.t *)
let getColor (v, c) = c

(* val toString : t -> string *)
let toString (v, c) =
  Printf.sprintf "%s%s" (Value.toString v) (Color.toString c)

(* val toStringVerbose : t -> string *)
let toStringVerbose (v, c) =
  Printf.sprintf "Card(%s, %s)" (Value.toStringVerbose v) (Color.toStringVerbose c)

(* val compare : t -> t -> int *)
let compare c1 c2 =
  ((getValue c1 |> Value.toInt) - (getValue c2 |> Value.toInt))

(* val max : t -> t -> t *)
let max c1 c2 =
  if compare c1 c2 < 0 then c2
  else c1

(* val min : t -> t -> t *)
let min c1 c2 =
  if compare c1 c2 <= 0 then c1
  else c2

(* val best : t list -> t *)
let best = function
  | [] -> invalid_arg "Card.best: list cannot be empty"
  | [e] -> e
  | e :: t -> List.fold_left max e t

(* val isOf : t -> Color.t -> bool *)
let isOf card c = (getColor card) = c

(* val isSpade : t -> bool *)
let isSpade c = isOf c Color.Spade

(* val isHeart : t -> bool *)
let isHeart c = isOf c Color.Heart

(* val isDiamond : t -> bool *)
let isDiamond c = isOf c Color.Diamond

(* val isClub : t -> bool *)
let isClub c = isOf c Color.Club
