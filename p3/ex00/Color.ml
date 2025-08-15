type color = Spade | Heart | Diamond | Club

(** The list of all values of type t *)
let all = [Spade; Heart; Diamond; Club]

(** "S", "H", "D" or "C" *)
let toString = function
  | Spade -> "S"
  | Heart -> "H"
  | Diamond -> "D"
  | Club -> "C"

(** "Spade", "Heart", etc *)
let toStringVerbose = function
  | Spade -> "Spade"
  | Heart -> "Heart"
  | Diamond -> "Diamond"
  | Club -> "Club"
