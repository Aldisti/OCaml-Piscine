
let ft_sum (f: int -> float) (lower: int) (upper: int) : float =
  let rec loop = fun (f: int -> float) lb lu acc -> (
    if lb > lu then acc
    else loop f (lb + 1) lu (acc +. (f lb))
  ) in
  if upper < lower then nan
  else loop f lower upper 0.0

(* TEST SUITE *)

let () = (
  let test actual expected =
    Printf.printf "actual: %.1f expected: %.1f\n" actual expected
  in
  let func1 = fun i -> float_of_int (i * i) in
  test (ft_sum func1 1 10) 385.0;
  test (ft_sum func1 2 5) 54.0;
  test (ft_sum func1 4 (-1)) nan;
)
