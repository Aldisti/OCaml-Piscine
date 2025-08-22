
let rec ft_power (n: int) (e: int) : int =
  if (e = 0) then 1
  else n * (ft_power n (e -1))

(* TEST SUITE *)

let test n e : unit =
  print_int n;
  print_string " ^ ";
  print_int e;
  print_string " => ";
  print_int (ft_power n e);
  print_char '\n'

let main unit =
  test 2 3;
  test 5 0;
  test 3 4;
  test 0 5;
  test (-2) 3

let () = main ()
