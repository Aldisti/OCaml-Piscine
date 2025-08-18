
let add (a: float) (b: float) : float = (+.) a b

let test a b =
  Printf.printf "%.2f + %.2f = %.2f\n" a b (add a b)

let () =
  test 4.5 2.3;
  test 1.2 1.69;
  test 3.6 0.
