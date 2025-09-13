
module type Watchtower = sig
  type hour = int
  val zero : hour
  val add : hour -> hour -> hour
  val sub : hour -> hour -> hour
end

module Watchtower : Watchtower = struct
  type hour = int

  let zero = 12

  let add a b =
    let h = (a + b) mod 12 in
    if h = 0 then zero else h

  let sub a b =
    let partial = (a mod 12) - (b mod 12) in
    if partial <= 0 then partial + zero else partial
end

let () =
  let test a b op f =
    Printf.printf "%d %s %d = %d\n" a op b (f a b)
  in
  let test_add a b = test a b "+" Watchtower.add in
  let test_sub a b = test a b "-" Watchtower.sub in
  print_endline "----- add -----";
  test_add 5 12;
  test_add 12 5;
  test_add 5 10;
  test_add 1 11;
  test_add 0 0;
  print_endline "----- sub -----";
  test_sub 12 5;
  test_sub 5 12;
  test_sub 8 7;
  test_sub 9 9;
