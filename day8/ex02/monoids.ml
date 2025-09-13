module type MONOID = sig
  type element
  val zero1 : element
  val zero2 : element
  val mul : element -> element -> element
  val add : element -> element -> element
  val div : element -> element -> element
  val sub : element -> element -> element
end

module Calc =
  functor (M : MONOID) -> struct
    let add a b = M.add a b
    let sub a b = M.sub a b
    let mul a b = M.mul a b
    let div a b = M.div a b

    let rec power (x: M.element) (e: int) : M.element =
      if e = 0 then M.zero2
      else M.mul x (power x (e - 1))

    let rec fact (x: M.element) : M.element =
      if x = M.zero1 then M.zero2
      else M.mul x (fact (M.sub x M.zero2))
  end

module INT = struct
  type element = int
  let zero1 = 0
  let zero2 = 1
  let add a b = a + b
  let sub a b = a - b
  let mul a b = a * b
  let div a b = a / b
end

module FLOAT = struct
  type element = float
  let zero1 = 0.
  let zero2 = 1.
  let add a b = a +. b
  let sub a b = a -. b
  let mul a b = a *. b
  let div a b = a /. b
end

module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)

let test_int a b op f =
  Printf.printf "%d %s %d = %d\n" a op b (f a b)

let test_float a b op f =
  Printf.printf "%.2f %s %.2f = %.2f\n" a op b (f a b)

let () =

  print_endline "----- int -----";
  test_int 11 31 "+" Calc_int.add;
  test_int 77 35 "-" Calc_int.sub;
  test_int 7 6 "*" Calc_int.mul;
  test_int 126 3 "/" Calc_int.div;
  test_int 2 5 "^" Calc_int.power;
  Printf.printf "%d! = %d\n" 5 (Calc_int.fact 5);

  print_endline "----- float -----";
  test_float 11. 31. "+" Calc_float.add;
  test_float 77. 35. "-" Calc_float.sub;
  test_float 7. 6. "*" Calc_float.mul;
  test_float 126. 3. "/" Calc_float.div;
  Printf.printf "%.2f ^ %d = %.2f\n" 2. 5 (Calc_float.power 2. 5);
  Printf.printf "%.2f! = %.2f\n" 5. (Calc_float.fact 5.);

  print_endline "----- others -----";
  print_endline (string_of_int (Calc_int.power 3 3));
  print_endline (string_of_float (Calc_float.power 3.0 3));
  print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
  print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0))
