
let ft_print_alphabet unit : unit =
  let rec loop = fun s e ->
    if s >= e then ()
    else (
      print_char (char_of_int s);
      loop (s + 1) e
    ) in
  loop 97 (97 + 26);
  print_char '\n'

(* TEST SUITE *)

let () = ft_print_alphabet ()
