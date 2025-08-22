
module type PAIR = sig val pair : (int * int) end

module type VAL = sig val x : int end

(* MY IMPLEMENTATION *)

module type MAKEPROJECTION = functor (P: PAIR) -> VAL

module MakeFst : MAKEPROJECTION =
  functor (P: PAIR) -> struct
    let x = Stdlib.fst P.pair
  end

module MakeSnd : MAKEPROJECTION =
  functor (P: PAIR) -> struct
    let x = Stdlib.snd P.pair
  end

(* END OF IMPLEMENTATION *)

module Pair : PAIR = struct
  let pair = ( 21, 42 )
end

module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)

let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x
