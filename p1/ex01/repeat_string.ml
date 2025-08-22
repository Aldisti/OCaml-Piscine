
let repeat_string ?(str="x") (n: int) : string =
  let rec loop acc i =
    if i <= 0 then acc
    else loop (acc ^ str) (i - 1)
  in
  if n < 0 then "Error"
  else loop "" n

(* TEST SUITE *)

let () =
  print_endline (repeat_string ~str:"a" (-1));
  print_endline (repeat_string 5);
  print_endline (repeat_string ~str:"What" 3);
  print_endline (repeat_string ~str:"foo" 1);
  print_endline (repeat_string ~str:"" 3);
  print_endline (repeat_string ~str:"Say it again!?" 0);

