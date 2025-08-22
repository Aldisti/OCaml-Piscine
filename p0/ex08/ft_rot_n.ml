
let ft_rot_n (n: int) (s: string) : string =
  let map_char c n a =
    char_of_int (((int_of_char c) - a + n) mod 26 + a)
  in
  String.map (fun c -> (
    if c >= 'a' && c <= 'z' then
      map_char c n 97
    else if c >= 'A' && c <= 'Z' then
      map_char c n 65
    else c
  )) s

(* TEST SUITE *)

let test n s : unit =
  print_char '|';
  print_string s;
  print_string "| => |";
  print_string (ft_rot_n n s);
  print_string "|\n"

let () =
  test 1 "abcdefghijklmnopqrstuvwxyz";
  test 13 "abcdefghijklmnopqrstuvwxyz";
  test 42 "0123456789";
  test 2 "OI2EAS67B9";
  test 0 "Damned !";
  test 42 "";
  test 1 "NBzlk qnbjr !"
