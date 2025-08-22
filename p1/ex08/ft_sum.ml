
let ft_sum (f: int -> float) (lower: int) (upper: int) : float =
  let rec loop (f: int -> float) i acc =
    if i > upper then acc
    else loop f (i + 1) (acc +. (f i))
  in
  if upper < lower then nan
  else loop f lower 0.0

(* TEST SUITE *)

let () =
  let test actual expected =
    Printf.printf "actual: %.1f expected: %.1f\n" actual expected
  in
  let func1 i = float_of_int (i * i) in
  let func2 i = float_of_int (i) in
  test (ft_sum func1 1 10) 385.0;
  test (ft_sum func1 2 5) 54.0;
  test (ft_sum func1 4 (-1)) nan;
  test (ft_sum func2 0 100) 5050.;
