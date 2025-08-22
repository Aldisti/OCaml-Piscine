
let ft_test_sign (n: int) : unit =
  if n >= 0 then print_endline "positive"
  else print_endline "negative"

(* TEST SUITE *)

let () =
  ft_test_sign 42;
  ft_test_sign 0;
  ft_test_sign (-42)
