
let rec repeat_string ?(str="x") (n: int) : string =
  if n < 0 then "Error"
  else if n == 0 then ""
  else str ^ (repeat_string ~str:str (n - 1))

(* TEST SUITE *)

let () = (
  print_endline (repeat_string 5);
  print_endline (repeat_string ~str:"a" 3);
  print_endline (repeat_string ~str:"a" (-1));
)
