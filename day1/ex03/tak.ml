
let rec tak x y z : int =
  if y >= x then z else
    tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)

(* TEST SUITE *)

let () = (
  let test x y z res =
    Printf.printf "%d %d %d => %d == %d\n" x y z (tak x y z) res
  in
  test 1 2 3 3;
  test 5 23 7 7;
  test 9 1 0 1;
  test 1 1 1 1;
  test 0 42 0 0;
  test 23498 98734 98776 98776;
)
