
let ft_rot_n (n: int) (s: string) : string =
  String.map (fun c -> (
    if c >= 'a' && c <= 'z' then
      char_of_int (((int_of_char c) - 97 + n) mod 26 + 97)
    else if c >= 'A' && c <= 'Z' then
      char_of_int (((int_of_char c) - 65 + n) mod 26 + 65)
    else c
  )) s

(* TEST SUITE *)

let test s1 s2 : unit =
  print_char '|';
  print_string s1;
  print_char '|';
  print_string " == ";
  print_char '|';
  print_string s2;
  print_string "|\n"

let main unit =
  test (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz") "bcdefghijklmnopqrstuvwxyza";
  test (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz") "nopqrstuvwxyzabcdefghijklm";
  test (ft_rot_n 42 "0123456789") "0123456789";
  test (ft_rot_n 2 "OI2EAS67B9") "QK2GCU67D9";
  test (ft_rot_n 0 "Damned !") "Damned !";
  test (ft_rot_n 42 "") "";
  test (ft_rot_n 1 "NBzlk qnbjr !") "OCaml rocks !"

let () = main ()
